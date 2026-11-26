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
      (setq modules '("src/sh_store.lsp"
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

(defun sh-run-dialog (/ *error* prevSS prevSnap slotIdx)
  (defun *error* (msg)
    (if sh-cleanup-temp-dcl
      (sh-cleanup-temp-dcl)
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
      ;; Harvest any deferred in-command _P selection before displaying history
      (sh-harvest-previous *sh-last-cmd*)

      ;; Snapshot any currently highlighted grips so Cancel restores them cleanly
      (setq prevSS (ssget "_I"))
      (if (and prevSS (> (sslength prevSS) 0))
        (setq prevSnap (sh-ss->snapshot prevSS "Pickfirst"))
      )

      (setq slotIdx (sh-show-dialog))
      (if slotIdx
        (sh-recall-slot slotIdx nil)
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
        )
      )
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; Direct Slot Command Runner (SH1 .. SH5)
;; --------------------------------------------------------------------------

(defun sh-run-slot (idx / *error*)
  (defun *error* (msg)
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
      (sh-harvest-previous *sh-last-cmd*)
      (sh-recall-slot idx nil)
    )
    (princ "\nSelectionHistory could not start because required modules failed to load.")
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMANDS: SH, SELHIST, SH1 .. SH5
;; --------------------------------------------------------------------------

(defun c:SELHIST () (sh-run-dialog))
(defun c:SH () (sh-run-dialog))
(defun c:SH1 () (sh-run-slot 1))
(defun c:SH2 () (sh-run-slot 2))
(defun c:SH3 () (sh-run-slot 3))
(defun c:SH4 () (sh-run-slot 4))
(defun c:SH5 () (sh-run-slot 5))

;; --------------------------------------------------------------------------
;; TRANSPARENT MID-COMMAND RECALL: (sh 1) .. (sh 5), (sh1) .. (sh5)
;; Use directly at any "Select objects:" prompt (returns a <Selection set>).
;; --------------------------------------------------------------------------

(defun sh (idx)
  (if (not *sh-loaded*)
    (progn
      (setq *sh-loaded* (sh-load-modules))
      (if *sh-loaded* (sh-install-reactors))
    )
  )
  (if *sh-loaded*
    (progn
      (sh-harvest-previous *sh-last-cmd*)
      (sh-recall-slot idx T)
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
  (princ "\nSelectionHistory loaded (tracking last 5 selections). Use SH, SH1-SH5, or (sh 1)-(sh 5).")
)
(princ)
