;;; ==========================================================================
;;; src/rt_cad.lsp - ZWCAD Boundary Detection, Polyline Preview & MTEXT/XDATA
;;; ==========================================================================

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

;; Clear the preview polyline: delete it unless keepBoundary is non-nil
;; (when kept, restore ByLayer color and remove highlight).
(defun rt-clear-preview (ent keepBoundary / obj)
  (if (and ent (entget ent))
    (if keepBoundary
      (progn
        (setq obj (vlax-ename->vla-object ent))
        (vl-catch-all-apply 'vla-put-Color (list obj 256)) ; ByLayer
        (vl-catch-all-apply 'vla-Update (list obj))
        (redraw ent 4) ; Unhighlight
      )
      (entdel ent)
    )
  )
)

;; Create centered MTEXT entity with ROOMTAG_DATA XDATA attached.
;; Returns the new MTEXT entity name (ename) or nil on failure.
;; XDATA layout (backward-compatible with v1 3-item tags):
;;   0: (1000 . roomName)   - e.g. "Cafe#1"
;;   1: (1000 . rtype)      - e.g. "Cafe"
;;   2: (1040 . areaM2)     - final adjusted & rounded area (m2)
;;   3: (1070 . chwFlag)    - 1 if CHW enabled, 0 if disabled
;;   4: (1040 . chwGpm)     - CHW flow in gpm (0.0 if disabled)
;;   5: (1040 . tonFactor)  - Area Ton Factor used (0.0 if disabled)
(defun rt-create-room-mtext (pt height labelStr roomName rtype areaM2 chwEnabled chwGpm tonFactor)
  (regapp "ROOMTAG_DATA")
  (if
    (entmake
      (list
        '(0 . "MTEXT")
        '(100 . "AcDbEntity")
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
          )
        )
      )
    )
    (entlast)
    nil
  )
)

(princ)
