;;; ==========================================================================
;;; SmartFilter.lsp - Entry Point & Module Loader for SmartFilter (ZWCAD)
;;; Commands: SF, SMARTFILTER
;;; ==========================================================================

(vl-load-com)

;; --------------------------------------------------------------------------
;; Module Loader
;; --------------------------------------------------------------------------

(defun sf-ensure-trailing-slash (dir)
  (if (and dir (> (strlen dir) 0))
    (if (member (substr dir (strlen dir) 1) '("\\" "/"))
      dir
      (strcat dir "/")
    )
    ""
  )
)

(defun sf-resolve-root-dir (/ p)
  (cond
    ((and *sf-root-dir* (findfile (strcat *sf-root-dir* "src/sf_types.lsp")))
     *sf-root-dir*
    )
    ((setq p (findfile "SmartFilter.lsp"))
     (sf-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "SmartFilter/SmartFilter.lsp"))
     (sf-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "src/sf_types.lsp"))
     (sf-ensure-trailing-slash (vl-filename-directory (vl-filename-directory p)))
    )
    ((findfile (strcat (getvar "DWGPREFIX") "src/sf_types.lsp"))
     (sf-ensure-trailing-slash (getvar "DWGPREFIX"))
    )
    ((setq p (getfiled "Locate SmartFilter.lsp (add folder to Support Path to skip)" "SmartFilter.lsp" "lsp" 0))
     (sf-ensure-trailing-slash (vl-filename-directory p))
    )
    (T nil)
  )
)

(defun sf-load-modules (/ modules ok modPath)
  (setq *sf-root-dir* (sf-resolve-root-dir))
  (if (null *sf-root-dir*)
    (progn
      (princ "\nError: Could not locate SmartFilter src/ modules.")
      nil
    )
    (progn
      (setq modules '("src/sf_types.lsp"
                      "src/sf_collect.lsp"
                      "src/sf_dialog.lsp")
            ok      T
      )
      (foreach m modules
        (setq modPath (strcat *sf-root-dir* m))
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

(setq *sf-loaded* (sf-load-modules))

;; --------------------------------------------------------------------------
;; Core Command Runner
;; --------------------------------------------------------------------------

(defun sf-run (/ *error* oldEcho ss hadPickfirst grouped groups totalCount
                 flags filteredSS selCount)
  (defun *error* (msg)
    (if sf-cleanup-temp-dcl
      (sf-cleanup-temp-dcl)
    )
    (if (and hadPickfirst ss)
      (sssetfirst nil ss)
    )
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (if (not *sf-loaded*)
    (setq *sf-loaded* (sf-load-modules))
  )

  (if (not *sf-loaded*)
    (princ "\nSmartFilter could not start because required modules failed to load.")
    (progn
      (setq oldEcho      (getvar "CMDECHO")
            hadPickfirst nil
            ss           (ssget "_I")
      )
      (setvar "CMDECHO" 0)

      (if (and ss (> (sslength ss) 0))
        (setq hadPickfirst T)
        (progn
          (princ "\nSelect objects to filter: ")
          (setq ss (ssget))
        )
      )

      (if (or (null ss) (= (sslength ss) 0))
        (princ "\nSmartFilter: No objects selected.")
        (progn
          (setq grouped    (sf-group-selection ss)
                groups     (car grouped)
                totalCount (cadr grouped)
                flags      (sf-show-dialog groups totalCount)
          )
          (if (null flags)
            (progn
              (if hadPickfirst
                (sssetfirst nil ss)
              )
              (princ "\nSmartFilter: Cancelled.")
            )
            (progn
              (setq filteredSS (sf-build-selection groups flags)
                    selCount   (if filteredSS (sslength filteredSS) 0)
              )
              (if (> selCount 0)
                (sssetfirst nil filteredSS)
                (sssetfirst nil nil)
              )
              (princ (sf-format-summary groups flags totalCount selCount))
              (if (and (> selCount 0) (= (getvar "PICKFIRST") 0))
                (princ "\nNote: Set PICKFIRST to 1 to keep grips active after filtering.")
              )
            )
          )
        )
      )

      (if oldEcho (setvar "CMDECHO" oldEcho))
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMANDS: SF, SMARTFILTER
;; --------------------------------------------------------------------------

(defun c:SMARTFILTER () (sf-run))
(defun c:SF () (sf-run))

(if *sf-loaded*
  (princ "\nSmartFilter loaded. Type SF or SMARTFILTER to filter selection by object type.")
)
(princ)
