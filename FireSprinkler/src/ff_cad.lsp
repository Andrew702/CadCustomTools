;;; ==========================================================================
;;; FireSprinkler/src/ff_cad.lsp - Layers, Boundary Preview & MTEXT/XDATA
;;; ==========================================================================

;; Return sorted list of local drawing layer names (excludes Xref "|" layers),
;; prefixed with "<Current>" so the user can always follow CLAYER.
(defun ff-get-local-layers (/ lay name names)
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
(defun ff-resolve-layer (layerChoice)
  (if (and layerChoice
           (/= layerChoice "")
           (/= layerChoice "<Current>")
           (tblsearch "LAYER" layerChoice))
    layerChoice
    nil
  )
)

;; Detect closed boundary at picked point (Xref-compatible).
;; Deletes inner island boundaries and returns (list maxArea maxEnt).
(defun ff-get-area-at (pt / lastEnt e ents maxArea maxEnt a obj)
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
(defun ff-show-preview (ent / obj)
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
;; When kept, assign targetLayer (if specified), restore ByLayer color, unhighlight.
(defun ff-clear-preview (ent keepBoundary bndryLayer / obj resolvedLayer)
  (if (and ent (entget ent))
    (if keepBoundary
      (progn
        (setq obj           (vlax-ename->vla-object ent)
              resolvedLayer (ff-resolve-layer bndryLayer)
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

;; Create centered MTEXT entity with FFSPRINKLER_DATA XDATA attached.
;; XDATA layout:
;;   0: (1000 . roomName)      - e.g. "Office#1"
;;   1: (1000 . rtype)         - e.g. "Office"
;;   2: (1040 . areaM2)        - final adjusted & rounded area (m2)
;;   3: (1000 . hazardLabel)   - "Light", "Ordinary", "Extra", or "Custom"
;;   4: (1040 . covArea)       - coverage area per sprinkler (m2)
;;   5: (1070 . sprCount)      - integer sprinkler count (rounded up)
(defun ff-create-room-mtext (pt height labelStr roomName rtype areaM2
                             hazardLabel covArea sprCount mtextLayer
                             / resolvedLayer dxf)
  (regapp "FFSPRINKLER_DATA")
  (setq resolvedLayer (ff-resolve-layer mtextLayer))
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
          (list "FFSPRINKLER_DATA"
            (cons 1000 roomName)
            (cons 1000 rtype)
            (cons 1040 (float areaM2))
            (cons 1000 hazardLabel)
            (cons 1040 (float covArea))
            (cons 1070 sprCount)
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
