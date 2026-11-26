;;; ==========================================================================
;;; src/xlk_reactor.lsp - Visual LISP Reactor for Automatic Xref Filtering
;;; ==========================================================================

(setq *xlk-pf-reactor-tag* "XREFLOCK_PICKFIRST_REACTOR")

;; --------------------------------------------------------------------------
;; Protected Reactor Callback Entry Point
;; --------------------------------------------------------------------------

(defun xlk-on-pickfirst-modified (reactor args)
  (vl-catch-all-apply 'xlk-filter-pickfirst nil)
  nil
)

;; --------------------------------------------------------------------------
;; Reactor Installation & Reload-Safe Cleanup
;; --------------------------------------------------------------------------

;; Remove any existing reactors of reactorType matching our tag string.
(defun xlk-remove-tagged-reactors (reactorType tag / groups grp r)
  (setq groups (vlr-reactors reactorType))
  (foreach grp groups
    (foreach r (cdr grp)
      (if (and r (equal (vlr-data r) tag))
        (vlr-remove r)
      )
    )
  )
)

;; Remove the XrefLock reactor if present.
(defun xlk-remove-reactors ()
  (xlk-remove-tagged-reactors :vlr-miscellaneous-reactor *xlk-pf-reactor-tag*)
  (setq *xlk-pf-reactor* nil)
  T
)

;; Install (or cleanly reinstall) the XrefLock pickfirst reactor.
(defun xlk-install-reactors ()
  (xlk-remove-reactors)
  (setq *xlk-pf-reactor*
    (vlr-miscellaneous-reactor
      *xlk-pf-reactor-tag*
      '(
        (:vlr-pickfirstModified . xlk-on-pickfirst-modified)
      )
    )
  )
  T
)
