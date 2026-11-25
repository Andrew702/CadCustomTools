;;; ==========================================================================
;;; src/sf_dialog.lsp - Dynamic Revit-Style Checkbox Dialog (DCL)
;;; ==========================================================================

;; Escape backslash and double-quote characters for safe DCL string literals.
(defun sf-dcl-escape (str / i len ch out)
  (setq i   1
        len (strlen str)
        out ""
  )
  (while (<= i len)
    (setq ch (substr str i 1))
    (cond
      ((= ch "\\") (setq out (strcat out "\\\\")))
      ((= ch "\"") (setq out (strcat out "\\\"")))
      (T           (setq out (strcat out ch)))
    )
    (setq i (1+ i))
  )
  out
)

;; Delete temporary DCL file if it exists and clear global tracker.
(defun sf-cleanup-temp-dcl ()
  (if (and *sf-temp-dcl* (findfile *sf-temp-dcl*))
    (vl-file-delete *sf-temp-dcl*)
  )
  (setq *sf-temp-dcl* nil)
)

;; Generate a temporary .dcl file with one checkbox per object type group.
;; Automatically flows into multiple columns (max 14 rows/column) so large
;; sets of entity types never overflow screen height.
;; Returns the temp file path, or nil on failure.
(defun sf-write-temp-dcl (groups / dclPath fh maxRows idx colCount g label cnt line)
  (sf-cleanup-temp-dcl)
  (setq dclPath (vl-filename-mktemp "sf_dlg_" nil ".dcl"))
  (if (or (null dclPath) (null (setq fh (open dclPath "w"))))
    nil
    (progn
      (setq *sf-temp-dcl* dclPath
            maxRows       14
            idx           0
            colCount      0
      )
      (write-line "smartfilter_dlg : dialog {" fh)
      (write-line "  label = \"Smart Selection Filter\";" fh)
      (write-line "  : row {" fh)
      (write-line "    : boxed_row {" fh)
      (write-line "      label = \"Object Types\";" fh)
      (write-line "      : column {" fh)

      (foreach g groups
        (if (and (> idx 0) (= (rem idx maxRows) 0))
          (progn
            (write-line "        : spacer { height = 0; }" fh)
            (write-line "      }" fh)
            (write-line "      : column {" fh)
            (setq colCount (1+ colCount))
          )
        )
        (setq label (sf-dcl-escape (car g))
              cnt   (cadr g)
              line  (strcat "        : toggle { key = \"t_" (itoa idx)
                            "\"; label = \"" label " (" (itoa cnt) ")\"; }"
                    )
        )
        (write-line line fh)
        (setq idx (1+ idx))
      )

      (write-line "        : spacer { height = 0; }" fh)
      (write-line "      }" fh)
      (write-line "    }" fh)

      ;; Right-hand action buttons (Revit Filter layout)
      (write-line "    : column {" fh)
      (write-line "      fixed_width = true;" fh)
      (write-line "      : button { key = \"btn_all\";  label = \"&Check All\";  width = 14; fixed_width = true; }" fh)
      (write-line "      : button { key = \"btn_none\"; label = \"Check &None\"; width = 14; fixed_width = true; }" fh)
      (write-line "      : button { key = \"btn_inv\";  label = \"&Invert\";     width = 14; fixed_width = true; }" fh)
      (write-line "      : spacer { height = 1; }" fh)
      (write-line "    }" fh)
      (write-line "  }" fh)

      ;; Live selection count footer
      (write-line "  : text {" fh)
      (write-line "    key = \"status_txt\";" fh)
      (write-line "    width = 52;" fh)
      (write-line "    label = \"\";" fh)
      (write-line "  }" fh)

      ;; OK / Cancel buttons
      (write-line "  : row {" fh)
      (write-line "    fixed_width = true;" fh)
      (write-line "    alignment = centered;" fh)
      (write-line "    : button { key = \"accept\"; label = \"OK\";     is_default = true; width = 12; fixed_width = true; }" fh)
      (write-line "    : button { key = \"cancel\"; label = \"Cancel\"; is_cancel  = true; width = 12; fixed_width = true; }" fh)
      (write-line "  }" fh)
      (write-line "}" fh)

      (close fh)
      dclPath
    )
  )
)

;; Update live "Selected: X of Y objects" status line inside the active dialog.
(defun sf-dlg-update-status (/ i sel cnts msg)
  (setq i    0
        sel  0
        cnts *sf-dlg-counts*
  )
  (while cnts
    (if (= (get_tile (strcat "t_" (itoa i))) "1")
      (setq sel (+ sel (car cnts)))
    )
    (setq i    (1+ i)
          cnts (cdr cnts)
    )
  )
  (setq msg
    (if (= sel 0)
      (strcat "Selected: 0 of " (itoa *sf-dlg-total*) " objects (selection will be cleared)")
      (strcat "Selected: " (itoa sel) " of " (itoa *sf-dlg-total*) " objects")
    )
  )
  (set_tile "status_txt" msg)
)

;; Set all checkboxes to "1" (Check All) or "0" (Check None).
(defun sf-dlg-set-all (val / i n)
  (setq i 0
        n (length *sf-dlg-counts*)
  )
  (while (< i n)
    (set_tile (strcat "t_" (itoa i)) val)
    (setq i (1+ i))
  )
  (sf-dlg-update-status)
)

;; Invert all checkboxes.
(defun sf-dlg-invert (/ i n key cur)
  (setq i 0
        n (length *sf-dlg-counts*)
  )
  (while (< i n)
    (setq key (strcat "t_" (itoa i))
          cur (get_tile key)
    )
    (set_tile key (if (= cur "1") "0" "1"))
    (setq i (1+ i))
  )
  (sf-dlg-update-status)
)

;; Capture checkbox states before closing dialog on OK.
(defun sf-dlg-on-accept (/ i n flags)
  (setq i     0
        n     (length *sf-dlg-counts*)
        flags nil
  )
  (while (< i n)
    (setq flags (cons (get_tile (strcat "t_" (itoa i))) flags)
          i     (1+ i)
    )
  )
  (setq *sf-dlg-result* (reverse flags))
  (done_dialog 1)
)

;; Display the Revit-style filter modal dialog.
;; Returns a list of "1"/"0" strings parallel to `groups` on OK, or nil on Cancel.
(defun sf-show-dialog (groups totalCount / dclPath dclId n i rc res)
  (setq *sf-dlg-counts* (mapcar 'cadr groups)
        *sf-dlg-total*  totalCount
        *sf-dlg-result* nil
        dclPath         (sf-write-temp-dcl groups)
  )
  (if (null dclPath)
    (progn
      (princ "\nError: SmartFilter could not create temporary DCL file.")
      nil
    )
    (progn
      (setq dclId (load_dialog dclPath))
      (if (or (null dclId) (< dclId 0))
        (progn
          (sf-cleanup-temp-dcl)
          (princ "\nError: SmartFilter could not load dialog definition.")
          nil
        )
        (progn
          (if (not (new_dialog "smartfilter_dlg" dclId))
            (progn
              (unload_dialog dclId)
              (sf-cleanup-temp-dcl)
              (princ "\nError: SmartFilter could not initialize dialog.")
              nil
            )
            (progn
              ;; Initialize all checkboxes as checked and bind toggle callbacks
              (setq n (length groups)
                    i 0
              )
              (while (< i n)
                (set_tile (strcat "t_" (itoa i)) "1")
                (action_tile (strcat "t_" (itoa i)) "(sf-dlg-update-status)")
                (setq i (1+ i))
              )
              (sf-dlg-update-status)

              ;; Action button callbacks
              (action_tile "btn_all"  "(sf-dlg-set-all \"1\")")
              (action_tile "btn_none" "(sf-dlg-set-all \"0\")")
              (action_tile "btn_inv"  "(sf-dlg-invert)")
              (action_tile "accept"   "(sf-dlg-on-accept)")
              (action_tile "cancel"   "(done_dialog 0)")

              (setq rc (start_dialog))
              (unload_dialog dclId)
              (sf-cleanup-temp-dcl)

              (setq res (if (= rc 1) *sf-dlg-result* nil))
              (setq *sf-dlg-counts* nil
                    *sf-dlg-total*  nil
                    *sf-dlg-result* nil
              )
              res
            )
          )
        )
      )
    )
  )
)

(princ)
