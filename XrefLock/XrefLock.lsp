;;; ==========================================================================
;;; XrefLock.lsp - Entry Point & Module Loader (ZWCAD)
;;; Commands: XREFLOCK, XRL, XREFLOCKON, XREFLOCKOFF
;;; ==========================================================================

(vl-load-com)

;; --------------------------------------------------------------------------
;; Module Loader
;; --------------------------------------------------------------------------

(defun xlk-ensure-trailing-slash (dir)
  (if (and dir (> (strlen dir) 0))
    (if (member (substr dir (strlen dir) 1) '("\\" "/"))
      dir
      (strcat dir "/")
    )
    ""
  )
)

(defun xlk-resolve-root-dir (/ p)
  (cond
    ((and *xlk-root-dir* (findfile (strcat *xlk-root-dir* "src/xlk_core.lsp")))
     *xlk-root-dir*
    )
    ((setq p (findfile "XrefLock.lsp"))
     (xlk-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "XrefLock/XrefLock.lsp"))
     (xlk-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "src/xlk_core.lsp"))
     (xlk-ensure-trailing-slash (vl-filename-directory (vl-filename-directory p)))
    )
    ((findfile (strcat (getvar "DWGPREFIX") "src/xlk_core.lsp"))
     (xlk-ensure-trailing-slash (getvar "DWGPREFIX"))
    )
    ((setq p (getfiled "Locate XrefLock.lsp (add folder to Support Path to skip)" "XrefLock.lsp" "lsp" 0))
     (xlk-ensure-trailing-slash (vl-filename-directory p))
    )
    (T nil)
  )
)

(defun xlk-load-modules (/ modules ok modPath)
  (setq *xlk-root-dir* (xlk-resolve-root-dir))
  (if (null *xlk-root-dir*)
    (progn
      (princ "\nError: Could not locate XrefLock src/ modules.")
      nil
    )
    (progn
      (setq modules '("src/xlk_core.lsp"
                      "src/xlk_reactor.lsp")
            ok      T
      )
      (foreach m modules
        (setq modPath (strcat *xlk-root-dir* m))
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

(setq *xlk-loaded* (xlk-load-modules))

;; Automatically install background reactor and apply initial state on load
(if *xlk-loaded*
  (progn
    (xlk-install-reactors)
    (xlk-set-active *xlk-active*)
  )
)

;; --------------------------------------------------------------------------
;; Command Helpers
;; --------------------------------------------------------------------------

(defun xlk-ensure-ready ()
  (if (not *xlk-loaded*)
    (progn
      (setq *xlk-loaded* (xlk-load-modules))
      (if *xlk-loaded*
        (xlk-install-reactors)
      )
    )
  )
  *xlk-loaded*
)

(defun xlk-print-status ()
  (princ
    (if *xlk-active*
      "\nXrefLock: LOCKED (Xrefs unselectable; hover preview disabled)."
      "\nXrefLock: UNLOCKED (Xrefs selectable)."
    )
  )
)

(defun xlk-run-set (mode / *error*)
  (defun *error* (msg)
    (setq *xlk-filtering* nil)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (if (not (xlk-ensure-ready))
    (princ "\nXrefLock could not start because required modules failed to load.")
    (progn
      (cond
        ((= mode 'TOGGLE) (xlk-set-active (not *xlk-active*)))
        ((= mode 'ON)     (xlk-set-active T))
        ((= mode 'OFF)    (xlk-set-active nil))
      )
      (xlk-print-status)
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMANDS: XREFLOCK, XRL, XREFLOCKON, XREFLOCKOFF
;; --------------------------------------------------------------------------

(defun c:XREFLOCK () (xlk-run-set 'TOGGLE))
(defun c:XRL () (xlk-run-set 'TOGGLE))
(defun c:XREFLOCKON () (xlk-run-set 'ON))
(defun c:XREFLOCKOFF () (xlk-run-set 'OFF))

(if *xlk-loaded*
  (princ
    (strcat
      "\nXrefLock loaded (Xref selection: "
      (if *xlk-active* "LOCKED" "UNLOCKED")
      "). Commands: XREFLOCK (XRL), XREFLOCKON, XREFLOCKOFF."
    )
  )
)
(princ)
