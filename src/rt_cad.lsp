;;; ==========================================================================
;;; src/rt_cad.lsp - ZWCAD Boundary Area Detection & MTEXT / XDATA Creation
;;; ==========================================================================

;; Detect closed boundary area at picked point (Xref-compatible).
;; Deletes temporary boundary entities unless keepBoundary is non-nil
;; (in which case the largest/outer boundary is kept).
(defun rt-get-area-at (pt keepBoundary / lastEnt e ents maxArea maxEnt a obj)
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
    (if (not (and keepBoundary (equal ent maxEnt)))
      (entdel ent)
    )
  )
  maxArea
)

;; Create centered MTEXT entity with ROOMTAG_DATA XDATA attached.
;; XDATA layout (backward-compatible with v1 3-item tags):
;;   0: (1000 . roomName)   - e.g. "Cafe#1"
;;   1: (1000 . rtype)      - e.g. "Cafe"
;;   2: (1040 . areaM2)     - final adjusted & rounded area (m2)
;;   3: (1070 . chwFlag)    - 1 if CHW enabled, 0 if disabled
;;   4: (1040 . chwGpm)     - CHW flow in gpm (0.0 if disabled)
;;   5: (1040 . tonFactor)  - Area Ton Factor used (0.0 if disabled)
(defun rt-create-room-mtext (pt height labelStr roomName rtype areaM2 chwEnabled chwGpm tonFactor)
  (regapp "ROOMTAG_DATA")
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
)

(princ)
