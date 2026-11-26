;;; ==========================================================================
;;; SelectionHistory.lsp - Entry Point & Module Loader (ZWCAD)
;;; Commands:    SH, SELHIST, SH1, SH2, SH3, SH4, SH5
;;; Transparent: (sh 1) .. (sh 5), (sh1) .. (sh5) at any "Select objects:" prompt
;;; ==========================================================================

(vl-load-com)

;; --------------------------------------------------------------------------
;; Module Loader
;; --------------------------------------------------------------------------

(defun sh-ensure-trailing-slash (dir)
  (if (and dir (> (strlen dir) 0))
    (if (member (substr dir (strlen dir) 1) '("\\" "/"))
      dir
      (strcat dir "/")
    )
    ""
  )
)

(defun sh-resolve-root-dir (/ p)
  (cond
    ((and *sh-root-dir* (findfile (strcat *sh-root-dir* "src/sh_store.lsp")))
     *sh-root-dir*
    )
    ((setq p (findfile "SelectionHistory.lsp"))
     (sh-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "SelectionHistory/SelectionHistory.lsp"))
     (sh-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "src/sh_store.lsp"))
     (sh-ensure-trailing-slash (vl-filename-directory (vl-filename-directory p)))
    )
    ((findfile (strcat (getvar "DWGPREFIX") "src/sh_store.lsp"))
     (sh-ensure-trailing-slash (getvar "DWGPREFIX"))
    )
    ((setq p (getfiled "Locate SelectionHistory.lsp (add folder to Support Path to skip)" "SelectionHistory.lsp" "lsp" 0))
     (sh-ensure-trailing-slash (vl-filename-directory p))
    )
    (T nil)
  )
)

(defun sh-load-modules (/ modules ok modPath)
  (setq *sh-root-dir* (sh-resolve-root-dir))
  (if (null *sh-root-dir*)
    (progn
      (princ "\nError: Could not locate SelectionHistory src/ modules.")
      nil
    )
    (progn
      (setq modules '("src/sh_log.lsp"
                      "src/sh_store.lsp"
                      "src/sh_reactor.lsp"
                      "src/sh_dialog.lsp")
            ok      T
      )
      (foreach m modules
        (setq modPath (strcat *sh-root-dir* m))
        (if (findfile modPath)
          (load modPath)
          (progn
            (princ (strcat "\nError: Missing module: " modPath))
            (setq ok nil)
          )
        )
      )
      (if sh-log-reactor-event
        (sh-log-reactor-event (if ok "INFO" "ERROR") "MODULE_LOAD"
          (list
            (cons "root_dir" *sh-root-dir*)
            (cons "ok"       (sh-log-bool ok))
          )
        )
      )
      ok
    )
  )
)

(setq *sh-loaded* (sh-load-modules))

;; Automatically install background reactors on load
(if *sh-loaded*
  (sh-install-reactors)
)

;; --------------------------------------------------------------------------
;; Interactive Picker Command Runner (SH / SELHIST)
;; --------------------------------------------------------------------------

(defun sh-run-dialog (/ *error* prevSS prevSnap slotIdx recalled)
  (defun *error* (msg)
    (if sh-cleanup-temp-dcl
      (sh-cleanup-temp-dcl)
    )
    (if sh-log-error
      (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (sh-log-error msg)
        (sh-log-end "cancelled" nil)
      )
    )
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (if (not *sh-loaded*)
    (progn
      (setq *sh-loaded* (sh-load-modules))
      (if *sh-loaded* (sh-install-reactors))
    )
  )

  (if (not *sh-loaded*)
    (princ "\nSelectionHistory could not start because required modules failed to load.")
    (progn
      (sh-log-begin "SH" nil)
      ;; Snapshot any currently highlighted grips so Cancel restores them cleanly
      (setq prevSS (ssget "_I"))
      (if (and prevSS (> (sslength prevSS) 0))
        (setq prevSnap (sh-ss->snapshot prevSS "Pickfirst"))
      )

      (setq slotIdx (sh-show-dialog))
      (if slotIdx
        (progn
          (setq recalled (sh-recall-slot slotIdx nil))
          (sh-log-end (if recalled "recalled" "recall_failed")
            (list (cons "slot" slotIdx))
          )
        )
        (progn
          (if (and prevSS (> (sslength prevSS) 0))
            (progn
              (if prevSnap
                (setq *sh-suppress-key* (cdr (assoc 'key prevSnap))
                      *sh-pending-pf*   nil
                )
              )
              (sssetfirst nil prevSS)
            )
          )
          (princ "\nSelection History: Cancelled.")
          (sh-log-end "cancelled"
            (list (cons "restored_pickfirst" (sh-log-bool (and prevSS (> (sslength prevSS) 0)))))
          )
        )
      )
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; Direct Slot Command Runner (SH1 .. SH5)
;; --------------------------------------------------------------------------

(defun sh-run-slot (idx / *error* recalled)
  (defun *error* (msg)
    (if sh-log-error
      (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (sh-log-error msg)
        (sh-log-end "cancelled" nil)
      )
    )
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (if (not *sh-loaded*)
    (progn
      (setq *sh-loaded* (sh-load-modules))
      (if *sh-loaded* (sh-install-reactors))
    )
  )

  (if *sh-loaded*
    (progn
      (sh-log-begin (strcat "SH" (if (numberp idx) (itoa idx) "")) (list (cons "slot" idx)))
      (setq recalled (sh-recall-slot idx nil))
      (sh-log-end (if recalled "recalled" "recall_failed") (list (cons "slot" idx)))
    )
    (princ "\nSelectionHistory could not start because required modules failed to load.")
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMANDS: SH, SELHIST, SH1 .. SH5, SHDEBUG
;; --------------------------------------------------------------------------

(defun c:SELHIST () (sh-run-dialog))
(defun c:SH () (sh-run-dialog))
(defun c:SH1 () (sh-run-slot 1))
(defun c:SH2 () (sh-run-slot 2))
(defun c:SH3 () (sh-run-slot 3))
(defun c:SH4 () (sh-run-slot 4))
(defun c:SH5 () (sh-run-slot 5))

(defun c:SHDEBUG ()
  (if (not *sh-loaded*)
    (progn
      (setq *sh-loaded* (sh-load-modules))
      (if *sh-loaded* (sh-install-reactors))
    )
  )
  (if *sh-loaded*
    (sh-log-debug-cmd)
    (princ "\nSelectionHistory modules are not loaded.")
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; TRANSPARENT MID-COMMAND RECALL: (sh 1) .. (sh 5), (sh1) .. (sh5)
;; Use directly at any "Select objects:" prompt (returns a <Selection set>).
;; --------------------------------------------------------------------------

(defun sh (idx / res)
  (if (not *sh-loaded*)
    (progn
      (setq *sh-loaded* (sh-load-modules))
      (if *sh-loaded* (sh-install-reactors))
    )
  )
  (if *sh-loaded*
    (progn
      (sh-log-begin "(sh)" (list (cons "slot" idx) (cons "transparent" 'true)))
      (setq res (sh-recall-slot idx T))
      (sh-log-end (if res "recalled" "recall_failed") (list (cons "slot" idx)))
      res
    )
    nil
  )
)

(defun sh1 () (sh 1))
(defun sh2 () (sh 2))
(defun sh3 () (sh 3))
(defun sh4 () (sh 4))
(defun sh5 () (sh 5))

(if *sh-loaded*
  (princ "\nSelectionHistory loaded (tracking last 5 selections). Use SH, SH1-SH5, (sh 1)-(sh 5), or SHDEBUG.")
)
(princ)
