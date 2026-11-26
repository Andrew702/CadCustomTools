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
      (setq modules '("src/sf_log.lsp"
                      "src/sf_types.lsp"
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
      (if sf-log-event
        (sf-log-event (if ok "INFO" "ERROR") "MODULE_LOAD"
          (list
            (cons "root_dir" *sf-root-dir*)
            (cons "ok"       (sf-log-bool ok))
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
                 sumGroupCnt sumLayerCnt layerBucketCnt g lg
                 flags filteredSS selCount summaryTxt)
  (defun *error* (msg)
    (if sf-cleanup-temp-dcl
      (sf-cleanup-temp-dcl)
    )
    (if (and hadPickfirst ss)
      (sssetfirst nil ss)
    )
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if sf-log-error
      (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (sf-log-error msg)
        (sf-log-end "cancelled" nil)
      )
    )
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
      (sf-log-begin "SMARTFILTER" nil)

      (if (and ss (> (sslength ss) 0))
        (setq hadPickfirst T)
        (progn
          (princ "\nSelect objects to filter: ")
          (setq ss (ssget))
        )
      )

      (sf-log-event "INFO" "SELECTION_INPUT"
        (list
          (cons "source"      (if hadPickfirst "pickfirst" "prompt"))
          (cons "total_count" (if ss (sslength ss) 0))
        )
      )

      (if (or (null ss) (= (sslength ss) 0))
        (progn
          (princ "\nSmartFilter: No objects selected.")
          (sf-log-end "empty_selection" nil)
        )
        (progn
          (setq grouped        (sf-group-selection ss)
                groups         (car grouped)
                totalCount     (cadr grouped)
                sumGroupCnt    0
                sumLayerCnt    0
                layerBucketCnt 0
          )
          (foreach g groups
            (setq sumGroupCnt (+ sumGroupCnt (cadr g)))
            (foreach lg (caddr g)
              (setq sumLayerCnt    (+ sumLayerCnt (cadr lg))
                    layerBucketCnt (1+ layerBucketCnt)
              )
            )
          )
          (sf-log-event "INFO" "GROUPS_BUILT"
            (list
              (cons "total_count"        totalCount)
              (cons "sum_group_counts"   sumGroupCnt)
              (cons "sum_layer_counts"   sumLayerCnt)
              (cons "type_count"         (length groups))
              (cons "layer_bucket_count" layerBucketCnt)
            )
          )
          (setq flags (sf-show-dialog groups totalCount))
          (if (null flags)
            (progn
              (if hadPickfirst
                (sssetfirst nil ss)
              )
              (sf-log-event "INFO" "FILTER_CANCELLED"
                (list
                  (cons "restored_pickfirst" (sf-log-bool hadPickfirst))
                  (cons "original_count"     totalCount)
                )
              )
              (princ "\nSmartFilter: Cancelled.")
              (sf-log-end "cancelled" nil)
            )
            (progn
              (setq filteredSS (sf-build-selection groups flags)
                    selCount   (if filteredSS (sslength filteredSS) 0)
                    summaryTxt (sf-format-summary groups flags totalCount selCount)
              )
              (if (> selCount 0)
                (sssetfirst nil filteredSS)
                (sssetfirst nil nil)
              )
              (sf-log-event "INFO" "FILTER_APPLIED"
                (list
                  (cons "total_count"      totalCount)
                  (cons "selected_count"   selCount)
                  (cons "by_layer"         (sf-log-bool (= *sf-by-layer* "1")))
                  (cons "pickfirst_sysvar" (getvar "PICKFIRST"))
                  (cons "summary"          (vl-string-trim "\r\n" summaryTxt))
                )
              )
              (princ summaryTxt)
              (if (and (> selCount 0) (= (getvar "PICKFIRST") 0))
                (princ "\nNote: Set PICKFIRST to 1 to keep grips active after filtering.")
              )
              (sf-log-end "applied"
                (list
                  (cons "total_count"    totalCount)
                  (cons "selected_count" selCount)
                )
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
;; COMMANDS: SF, SMARTFILTER, SFDEBUG
;; --------------------------------------------------------------------------

(defun c:SMARTFILTER () (sf-run))
(defun c:SF () (sf-run))

(defun c:SFDEBUG ()
  (if (not *sf-loaded*)
    (setq *sf-loaded* (sf-load-modules))
  )
  (if *sf-loaded*
    (sf-log-debug-cmd)
    (princ "\nSmartFilter modules are not loaded.")
  )
  (princ)
)

(if *sf-loaded*
  (princ "\nSmartFilter loaded. Commands: SF, SMARTFILTER, SFDEBUG")
)
(princ)
