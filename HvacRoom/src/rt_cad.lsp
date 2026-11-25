;;; ==========================================================================
;;; src/rt_cad.lsp - ZWCAD Layers, Boundary Preview & MTEXT/XDATA Creation
;;; ==========================================================================

;; Return sorted list of local drawing layer names (excludes Xref "|" layers),
;; prefixed with "<Current>" so the user can always follow CLAYER.
(defun rt-get-local-layers (/ lay name names)
  (setq lay (tblnext "LAYER" T))
  (while lay
    (setq name (cdr (assoc 2 lay)))
    (if (and name (not (vl-string-search "|" name)))
      (setq names (cons name names))
    )
    (setq lay (tblnext "LAYER"))
  )
  (cons "<Current>" (vl-sort names '(lambda (a b) (< (strcase a) (strcase b)))))
)

;; Resolve a layer choice ("<Current>" or explicit name) to an actual layer name
;; (returns nil if "<Current>" or if the layer does not exist in this drawing).
(defun rt-resolve-layer (layerChoice)
  (if (and layerChoice
           (/= layerChoice "")
           (/= layerChoice "<Current>")
           (tblsearch "LAYER" layerChoice))
    layerChoice
    nil
  )
)

;; Detect closed boundary at picked point (Xref-compatible).
;; Deletes any inner island boundaries and returns (list maxArea maxEnt),
;; leaving maxEnt alive so the caller can display a polyline preview and
;; later clean it up with rt-clear-preview. Returns nil on failure.
(defun rt-get-area-at (pt / lastEnt e ents maxArea maxEnt a obj)
  (setq lastEnt (entlast))
  (command "_.-boundary" "_non" pt "")
  (while (> (getvar "CMDACTIVE") 0)
    (command "")
  )
  (setq e (if lastEnt (entnext lastEnt) (entnext)))
  (while e
    (setq ents (cons e ents))
    (setq e (entnext e))
  )
  (setq maxArea nil
        maxEnt  nil
  )
  (foreach ent ents
    (setq obj (vlax-ename->vla-object ent))
    (setq a (vl-catch-all-apply 'vla-get-Area (list obj)))
    (if (and (not (vl-catch-all-error-p a)) (numberp a))
      (if (or (null maxArea) (> a maxArea))
        (setq maxArea a
              maxEnt  ent
        )
      )
    )
  )
  ;; Remove any secondary/inner boundary entities immediately
  (foreach ent ents
    (if (not (equal ent maxEnt))
      (entdel ent)
    )
  )
  (if (and maxArea maxEnt)
    (list maxArea maxEnt)
    nil
  )
)

;; Highlight the detected boundary polyline in bright green as a quick preview
(defun rt-show-preview (ent / obj)
  (if (and ent (entget ent))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (vl-catch-all-apply 'vla-put-Color (list obj 3)) ; ACI 3 = Green
      (vl-catch-all-apply 'vla-Update (list obj))
      (redraw ent 3) ; Highlight
    )
  )
)

;; Clear the preview polyline: delete it unless keepBoundary is non-nil.
;; When kept as a persistent area object, assign targetLayer (if specified),
;; restore ByLayer color, and remove the temporary highlight.
(defun rt-clear-preview (ent keepBoundary bndryLayer / obj resolvedLayer)
  (if (and ent (entget ent))
    (if keepBoundary
      (progn
        (setq obj           (vlax-ename->vla-object ent)
              resolvedLayer (rt-resolve-layer bndryLayer)
        )
        (if resolvedLayer
          (vl-catch-all-apply 'vla-put-Layer (list obj resolvedLayer))
        )
        (vl-catch-all-apply 'vla-put-Color (list obj 256)) ; ByLayer
        (vl-catch-all-apply 'vla-Update (list obj))
        (redraw ent 4) ; Unhighlight
      )
      (entdel ent)
    )
  )
)

;; Create centered MTEXT entity with ROOMTAG_DATA XDATA attached.
;; Places MTEXT on mtextLayer if specified (else current CLAYER).
;; Returns the new MTEXT entity name (ename) or nil on failure.
;; XDATA layout (backward-compatible with v1 3-item and v1.1 6-item tags):
;;   0: (1000 . roomName)   - e.g. "Cafe#1"
;;   1: (1000 . rtype)      - e.g. "Cafe"
;;   2: (1040 . areaM2)     - final adjusted & rounded area (m2)
;;   3: (1070 . chwFlag)    - 1 if CHW enabled, 0 if disabled
;;   4: (1040 . chwGpm)     - CHW flow in gpm (0.0 if disabled)
;;   5: (1040 . tonFactor)  - Area Ton Factor used (0.0 if disabled)
;;   6: (1070 . pipeFlag)   - 1 if pipe sizing enabled, 0 if disabled
;;   7: (1000 . pipeSize)   - e.g. "25mm" or "1\"" ("" if disabled)
(defun rt-create-room-mtext (pt height labelStr roomName rtype areaM2
                             chwEnabled chwGpm tonFactor pipeStr mtextLayer
                             / resolvedLayer hasPipe dxf)
  (regapp "ROOMTAG_DATA")
  (setq resolvedLayer (rt-resolve-layer mtextLayer)
        hasPipe       (and chwEnabled pipeStr (/= pipeStr ""))
  )
  (setq dxf
    (append
      (list
        '(0 . "MTEXT")
        '(100 . "AcDbEntity")
      )
      (if resolvedLayer
        (list (cons 8 resolvedLayer))
        nil
      )
      (list
        '(100 . "AcDbMText")
        (cons 10 (trans pt 1 0))
        (cons 40 (float height))
        '(71 . 5) ; Middle-Center attachment
        (cons 1 labelStr)
        (list -3
          (list "ROOMTAG_DATA"
            (cons 1000 roomName)
            (cons 1000 rtype)
            (cons 1040 (float areaM2))
            (cons 1070 (if chwEnabled 1 0))
            (cons 1040 (if chwEnabled (float chwGpm) 0.0))
            (cons 1040 (if chwEnabled (float tonFactor) 0.0))
            (cons 1070 (if hasPipe 1 0))
            (cons 1000 (if hasPipe pipeStr ""))
          )
        )
      )
    )
  )
  (if (entmake dxf)
    (entlast)
    nil
  )
)

(princ)
