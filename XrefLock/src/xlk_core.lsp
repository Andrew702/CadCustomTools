;;; ==========================================================================
;;; src/xlk_core.lsp - Xref Detection, Pickfirst Filtering & State Management
;;; ==========================================================================

;; Initialize persistent per-drawing state only once across reloads
(if (null *xlk-initialized*)
  (setq *xlk-initialized*    T
        *xlk-active*         T
        *xlk-filtering*      nil
        *xlk-saved-pf-state* nil
  )
)

;; --------------------------------------------------------------------------
;; Xref Detection
;; --------------------------------------------------------------------------

;; Return T if ename is an INSERT referencing an external reference (Xref).
;; DXF group 70 bit 4 on a BLOCK table record identifies both Attached and
;; Overlay Xrefs.
(defun xlk-xref-p (ename / edata blkname blkdata flags)
  (if (and ename
           (setq edata (entget ename))
           (= (cdr (assoc 0 edata)) "INSERT")
      )
    (progn
      (setq blkname (cdr (assoc 2 edata))
            blkdata (if blkname (tblsearch "BLOCK" blkname) nil)
            flags   (if blkdata (cdr (assoc 70 blkdata)) nil)
      )
      (and (numberp flags) (= 4 (logand 4 flags)))
    )
    nil
  )
)

;; --------------------------------------------------------------------------
;; Hover Preview Filter (PREVIEWFILTER bit 2 = Exclude Xrefs)
;; --------------------------------------------------------------------------

;; When locking Xrefs, ensure PREVIEWFILTER bit 2 is set so hovering over
;; Xref geometry does not highlight or lag the cursor. When unlocking,
;; restore bit 2 only if XrefLock was the one that enabled it.
(defun xlk-sync-previewfilter (active / cur)
  (setq cur (vl-catch-all-apply 'getvar '("PREVIEWFILTER")))
  (if (and (not (vl-catch-all-error-p cur)) (numberp cur))
    (if active
      (progn
        (if (null *xlk-saved-pf-state*)
          (setq *xlk-saved-pf-state*
            (if (= 2 (logand cur 2)) 'HAD_BIT 'NO_BIT)
          )
        )
        (if (/= 2 (logand cur 2))
          (vl-catch-all-apply 'setvar (list "PREVIEWFILTER" (logior cur 2)))
        )
      )
      (progn
        (if (and (= *xlk-saved-pf-state* 'NO_BIT) (= 2 (logand cur 2)))
          (vl-catch-all-apply 'setvar (list "PREVIEWFILTER" (- cur 2)))
        )
        (setq *xlk-saved-pf-state* nil)
      )
    )
  )
  nil
)

;; --------------------------------------------------------------------------
;; Pickfirst Selection Filtering
;; --------------------------------------------------------------------------

;; Apply filtered selection set to pickfirst grips.
(defun xlk-apply-pickfirst (newss)
  (if (and newss (> (sslength newss) 0))
    (sssetfirst nil newss)
    (sssetfirst nil nil)
  )
)

;; Strip any Xref entities from the current pickfirst selection set (_I).
;; Uses *xlk-filtering* as a re-entrancy guard because sssetfirst can
;; re-trigger :vlr-pickfirstModified.
(defun xlk-filter-pickfirst (/ ss i ename newss modified)
  (if (and *xlk-active*
           (not *xlk-filtering*)
           (setq ss (ssget "_I"))
           (> (sslength ss) 0)
      )
    (progn
      (setq newss    (ssadd)
            i        0
            modified nil
      )
      (repeat (sslength ss)
        (setq ename (ssname ss i))
        (if (xlk-xref-p ename)
          (setq modified T)
          (ssadd ename newss)
        )
        (setq i (1+ i))
      )
      (if modified
        (progn
          (setq *xlk-filtering* T)
          (vl-catch-all-apply 'xlk-apply-pickfirst (list newss))
          (setq *xlk-filtering* nil)
        )
      )
    )
  )
  nil
)

;; --------------------------------------------------------------------------
;; State Control
;; --------------------------------------------------------------------------

;; Set XrefLock active state, synchronize PREVIEWFILTER, and immediately
;; drop any currently gripped Xrefs when locking.
(defun xlk-set-active (flag)
  (setq *xlk-active* (if flag T nil))
  (xlk-sync-previewfilter *xlk-active*)
  (if *xlk-active*
    (xlk-filter-pickfirst)
  )
  *xlk-active*
)
