;;; ==========================================================================
;;; src/sf_dialog.lsp - Dynamic Revit-Style Checkbox Dialog (Type + Layer DCL)
;;; ==========================================================================

;; Persistent session preference: "0" = Object Types only, "1" = Break down by Layer
(if (not *sf-by-layer*) (setq *sf-by-layer* "0"))

;; Escape backslash, double-quote, and ampersand characters for safe DCL literals.
;; In DCL, a single '&' underlines the next character as a mnemonic, so literal
;; ampersands in labels/layer names must be doubled as '&&'.
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
      ((= ch "&")  (setq out (strcat out "&&")))
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

;; Generate a temporary .dcl file for either compact Object Types mode or
;; expanded Object Types + indented Layer sub-options mode.
;; Uses fixed_width/alignment=left for crisp indentation, fixed_height/alignment=top
;; to prevent vertical stretching, and balanced multi-column distribution.
;; Returns the temp file path, or nil on failure.
(defun sf-write-temp-dcl (groups byLayer / dclPath fh totalRows maxPerCol numCols
                                           targetRows rowCount colIdx idx j g label
                                           cnt lGroups grpRows lg lLabel lCnt boxLabel)
  (sf-cleanup-temp-dcl)
  (setq dclPath (vl-filename-mktemp "sf_dlg_" nil ".dcl"))
  (if (or (null dclPath) (null (setq fh (open dclPath "w"))))
    nil
    (progn
      ;; Compute balanced column count and target rows per column
      (setq totalRows 0)
      (foreach g groups
        (setq totalRows
          (+ totalRows (if (= byLayer "1") (1+ (length (caddr g))) 1))
        )
      )
      (if (< totalRows 1) (setq totalRows 1))
      (setq *sf-temp-dcl* dclPath
            maxPerCol     (if (= byLayer "1") 17 14)
            numCols       (1+ (/ (1- totalRows) maxPerCol))
            targetRows    (1+ (/ (1- totalRows) numCols))
            rowCount      0
            colIdx        0
            idx           0
            boxLabel      (if (= byLayer "1") "Object Types && Layers" "Object Types")
      )

      (write-line "smartfilter_dlg : dialog {" fh)
      (write-line "  label = \"Smart Selection Filter\";" fh)

      ;; Top option row: toggle layer sub-options
      (write-line "  : row {" fh)
      (write-line "    fixed_width = true;" fh)
      (write-line "    alignment = left;" fh)
      (write-line "    : toggle { key = \"chk_layer\"; label = \"Break down by &Layer\"; fixed_width = true; alignment = left; }" fh)
      (write-line "  }" fh)

      ;; Main content row: boxed checkboxes on left, stacked actions on top-right
      (write-line "  : row {" fh)
      (write-line "    : boxed_row {" fh)
      (write-line (strcat "      label = \"" boxLabel "\";") fh)
      (write-line "      : column {" fh)
      (write-line "        fixed_height = true;" fh)
      (write-line "        alignment = top;" fh)

      (foreach g groups
        (setq label   (sf-dcl-escape (car g))
              cnt     (cadr g)
              lGroups (caddr g)
              grpRows (if (= byLayer "1") (1+ (length lGroups)) 1)
        )
        ;; Start a new balanced column when current column reaches target height,
        ;; avoiding an orphaned parent header at the bottom of a column.
        (if (and (> rowCount 0)
                 (< colIdx (1- numCols))
                 (or (>= rowCount targetRows)
                     (and (= byLayer "1")
                          (>= (+ rowCount 2) targetRows)
                          (> (+ rowCount grpRows) (+ targetRows 2))
                     )
                 )
            )
          (progn
            (write-line "      }" fh)
            (write-line "      : spacer { width = 2; fixed_width = true; }" fh)
            (write-line "      : column {" fh)
            (write-line "        fixed_height = true;" fh)
            (write-line "        alignment = top;" fh)
            (setq rowCount 0
                  colIdx   (1+ colIdx)
            )
          )
        )

        ;; Subtle visual gap between object-type groups in layer mode
        (if (and (= byLayer "1") (> rowCount 0))
          (write-line "        : spacer { height = 0.35; fixed_height = true; }" fh)
        )

        ;; Parent object type checkbox (flush-left)
        (write-line
          (strcat "        : toggle { key = \"t_" (itoa idx)
                  "\"; label = \"" label " (" (itoa cnt)
                  ")\"; fixed_width = true; alignment = left; }"
          )
          fh
        )
        (setq rowCount (1+ rowCount))

        ;; Indented layer sub-checkboxes when Break down by Layer is enabled
        (if (= byLayer "1")
          (progn
            (setq j 0)
            (foreach lg lGroups
              (if (and (>= rowCount maxPerCol) (< colIdx (1- numCols)))
                (progn
                  (write-line "      }" fh)
                  (write-line "      : spacer { width = 2; fixed_width = true; }" fh)
                  (write-line "      : column {" fh)
                  (write-line "        fixed_height = true;" fh)
                  (write-line "        alignment = top;" fh)
                  (setq rowCount 0
                        colIdx   (1+ colIdx)
                  )
                )
              )
              (setq lLabel (sf-dcl-escape (car lg))
                    lCnt   (cadr lg)
              )
              (write-line "        : row {" fh)
              (write-line "          fixed_width = true;" fh)
              (write-line "          alignment = left;" fh)
              (write-line "          : spacer { width = 3; fixed_width = true; }" fh)
              (write-line
                (strcat "          : toggle { key = \"l_" (itoa idx) "_" (itoa j)
                        "\"; label = \"" lLabel " (" (itoa lCnt)
                        ")\"; fixed_width = true; alignment = left; }"
                )
                fh
              )
              (write-line "        }" fh)
              (setq rowCount (1+ rowCount)
                    j        (1+ j)
              )
            )
          )
        )
        (setq idx (1+ idx))
      )

      (write-line "      }" fh)
      (write-line "    }" fh)

      ;; Right-hand action buttons pinned tightly at top-right (Revit Filter layout)
      (write-line "    : column {" fh)
      (write-line "      fixed_width = true;" fh)
      (write-line "      fixed_height = true;" fh)
      (write-line "      alignment = top;" fh)
      (write-line "      : button { key = \"btn_all\";  label = \"&Check All\";  width = 14; fixed_width = true; }" fh)
      (write-line "      : button { key = \"btn_none\"; label = \"Check &None\"; width = 14; fixed_width = true; }" fh)
      (write-line "      : button { key = \"btn_inv\";  label = \"&Invert\";     width = 14; fixed_width = true; }" fh)
      (write-line "    }" fh)
      (write-line "  }" fh)

      ;; Unified bottom footer: live status on left, OK / Cancel on right
      (write-line "  : spacer { height = 0.25; fixed_height = true; }" fh)
      (write-line "  : row {" fh)
      (write-line "    : text {" fh)
      (write-line "      key = \"status_txt\";" fh)
      (write-line "      width = 44;" fh)
      (write-line "      alignment = left;" fh)
      (write-line "    }" fh)
      (write-line "    : row {" fh)
      (write-line "      fixed_width = true;" fh)
      (write-line "      alignment = right;" fh)
      (write-line "      : button { key = \"accept\"; label = \"OK\";     is_default = true; width = 11; fixed_width = true; }" fh)
      (write-line "      : button { key = \"cancel\"; label = \"Cancel\"; is_cancel  = true; width = 11; fixed_width = true; }" fh)
      (write-line "    }" fh)
      (write-line "  }" fh)
      (write-line "}" fh)

      (close fh)
      dclPath
    )
  )
)

;; Update live "Selected: X of Y objects" status line inside the active dialog.
(defun sf-dlg-update-status (/ i j sel groups g lGroups lg msg)
  (setq i      0
        sel    0
        groups *sf-dlg-groups*
  )
  (while groups
    (setq g (car groups))
    (if (= *sf-by-layer* "1")
      (progn
        (setq j       0
              lGroups (caddr g)
        )
        (while lGroups
          (setq lg (car lGroups))
          (if (= (get_tile (strcat "l_" (itoa i) "_" (itoa j))) "1")
            (setq sel (+ sel (cadr lg)))
          )
          (setq j       (1+ j)
                lGroups (cdr lGroups)
          )
        )
      )
      (if (= (get_tile (strcat "t_" (itoa i))) "1")
        (setq sel (+ sel (cadr g)))
      )
    )
    (setq i      (1+ i)
          groups (cdr groups)
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

;; Callback: parent object type toggled while in layer-breakdown mode.
;; Propagates parent check state to all its layer sub-checkboxes.
(defun sf-dlg-on-parent (i / val nLayers j)
  (if (= *sf-by-layer* "1")
    (progn
      (setq val     (get_tile (strcat "t_" (itoa i)))
            nLayers (length (caddr (nth i *sf-dlg-groups*)))
            j       0
      )
      (while (< j nLayers)
        (set_tile (strcat "l_" (itoa i) "_" (itoa j)) val)
        (setq j (1+ j))
      )
    )
  )
  (sf-dlg-update-status)
)

;; Callback: child layer checkbox toggled.
;; Syncs parent object type checkbox (checked iff at least one layer is checked).
(defun sf-dlg-on-layer (i / nLayers j anyOn)
  (setq nLayers (length (caddr (nth i *sf-dlg-groups*)))
        j       0
        anyOn   nil
  )
  (while (and (< j nLayers) (not anyOn))
    (if (= (get_tile (strcat "l_" (itoa i) "_" (itoa j))) "1")
      (setq anyOn T)
    )
    (setq j (1+ j))
  )
  (set_tile (strcat "t_" (itoa i)) (if anyOn "1" "0"))
  (sf-dlg-update-status)
)

;; Set all checkboxes (and layer sub-checkboxes if visible) to "1" or "0".
(defun sf-dlg-set-all (val / i j n nLayers)
  (setq i 0
        n (length *sf-dlg-groups*)
  )
  (while (< i n)
    (set_tile (strcat "t_" (itoa i)) val)
    (if (= *sf-by-layer* "1")
      (progn
        (setq nLayers (length (caddr (nth i *sf-dlg-groups*)))
              j       0
        )
        (while (< j nLayers)
          (set_tile (strcat "l_" (itoa i) "_" (itoa j)) val)
          (setq j (1+ j))
        )
      )
    )
    (setq i (1+ i))
  )
  (sf-dlg-update-status)
)

;; Invert all checkboxes (flips leaves in layer mode and syncs parents).
(defun sf-dlg-invert (/ i j n nLayers key cur anyOn)
  (setq i 0
        n (length *sf-dlg-groups*)
  )
  (while (< i n)
    (if (= *sf-by-layer* "1")
      (progn
        (setq nLayers (length (caddr (nth i *sf-dlg-groups*)))
              j       0
              anyOn   nil
        )
        (while (< j nLayers)
          (setq key (strcat "l_" (itoa i) "_" (itoa j))
                cur (get_tile key)
          )
          (if (= cur "1")
            (set_tile key "0")
            (progn
              (set_tile key "1")
              (setq anyOn T)
            )
          )
          (setq j (1+ j))
        )
        (set_tile (strcat "t_" (itoa i)) (if anyOn "1" "0"))
      )
      (progn
        (setq key (strcat "t_" (itoa i))
              cur (get_tile key)
        )
        (set_tile key (if (= cur "1") "0" "1"))
      )
    )
    (setq i (1+ i))
  )
  (sf-dlg-update-status)
)

;; Capture current checkbox state as nested per-layer flags ((f00 f01 ...) ...).
(defun sf-dlg-capture-flags (/ i j n nLayers tVal lFlags allFlags)
  (setq i        0
        n        (length *sf-dlg-groups*)
        allFlags nil
  )
  (while (< i n)
    (setq nLayers (length (caddr (nth i *sf-dlg-groups*)))
          j       0
          lFlags  nil
    )
    (if (= *sf-by-layer* "1")
      (while (< j nLayers)
        (setq lFlags (cons (get_tile (strcat "l_" (itoa i) "_" (itoa j))) lFlags)
              j      (1+ j)
        )
      )
      (progn
        (setq tVal (get_tile (strcat "t_" (itoa i))))
        (while (< j nLayers)
          (setq lFlags (cons tVal lFlags)
                j      (1+ j)
          )
        )
      )
    )
    (setq allFlags (cons (reverse lFlags) allFlags)
          i        (1+ i)
    )
  )
  (reverse allFlags)
)

;; Callback: "Break down by Layer" toggled - preserve current state and rebuild DCL.
(defun sf-dlg-on-layer-mode ()
  (setq *sf-dlg-layer-flags* (sf-dlg-capture-flags)
        *sf-by-layer*        (get_tile "chk_layer")
  )
  (done_dialog 2)
)

;; Callback: OK pressed - capture final nested per-layer flags and close.
(defun sf-dlg-on-accept ()
  (setq *sf-dlg-result* (sf-dlg-capture-flags))
  (done_dialog 1)
)

;; Build initial all-checked nested layer flags parallel to `groups`.
(defun sf-init-layer-flags (groups)
  (mapcar
    (function
      (lambda (g)
        (mapcar (function (lambda (lg) "1")) (caddr g))
      )
    )
    groups
  )
)

;; Display the Revit-style filter modal dialog (with dynamic layer-mode toggle).
;; Returns nested per-layer flags ((f00 f01 ...) ...) on OK, or nil on Cancel.
(defun sf-show-dialog (groups totalCount / dclPath dclId rc keepLoop n i j lFlags
                                           anyOn res)
  (setq *sf-dlg-groups*      groups
        *sf-dlg-total*       totalCount
        *sf-dlg-layer-flags* (sf-init-layer-flags groups)
        *sf-dlg-result*      nil
        keepLoop             T
  )

  (while keepLoop
    (setq dclPath (sf-write-temp-dcl groups *sf-by-layer*))
    (if (null dclPath)
      (progn
        (sf-log-event "ERROR" "DIALOG_ERROR" (list (cons "reason" "temp_dcl_write_failed")))
        (princ "\nError: SmartFilter could not create temporary DCL file.")
        (setq keepLoop nil
              res      nil
        )
      )
      (progn
        (setq dclId (load_dialog dclPath))
        (if (or (null dclId) (< dclId 0))
          (progn
            (sf-cleanup-temp-dcl)
            (sf-log-event "ERROR" "DIALOG_ERROR" (list (cons "reason" "load_dialog_failed") (cons "path" dclPath)))
            (princ "\nError: SmartFilter could not load dialog definition.")
            (setq keepLoop nil
                  res      nil
            )
          )
          (if (not (new_dialog "smartfilter_dlg" dclId))
            (progn
              (unload_dialog dclId)
              (sf-cleanup-temp-dcl)
              (sf-log-event "ERROR" "DIALOG_ERROR" (list (cons "reason" "new_dialog_failed") (cons "path" dclPath)))
              (princ "\nError: SmartFilter could not initialize dialog.")
              (setq keepLoop nil
                    res      nil
              )
            )
            (progn
              ;; Set layer-breakdown toggle state
              (set_tile "chk_layer" *sf-by-layer*)
              (action_tile "chk_layer" "(sf-dlg-on-layer-mode)")

              ;; Initialize object-type and (if expanded) layer checkboxes
              (setq n (length groups)
                    i 0
              )
              (while (< i n)
                (setq lFlags (nth i *sf-dlg-layer-flags*)
                      anyOn  (if (member "1" lFlags) T nil)
                )
                (set_tile (strcat "t_" (itoa i)) (if anyOn "1" "0"))
                (action_tile (strcat "t_" (itoa i))
                             (strcat "(sf-dlg-on-parent " (itoa i) ")")
                )
                (if (= *sf-by-layer* "1")
                  (progn
                    (setq j 0)
                    (foreach f lFlags
                      (set_tile (strcat "l_" (itoa i) "_" (itoa j)) f)
                      (action_tile (strcat "l_" (itoa i) "_" (itoa j))
                                   (strcat "(sf-dlg-on-layer " (itoa i) ")")
                      )
                      (setq j (1+ j))
                    )
                  )
                )
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

              (cond
                ((= rc 2)
                 ;; Rebuild dialog in toggled layer mode
                 (sf-log-event "INFO" "DIALOG_MODE_TOGGLE"
                   (list (cons "by_layer" (sf-log-bool (= *sf-by-layer* "1"))))
                 )
                 (setq keepLoop T)
                )
                ((= rc 1)
                 (sf-log-event "INFO" "DIALOG_CLOSE"
                   (list
                     (cons "rc"       rc)
                     (cons "outcome"  "ok")
                     (cons "by_layer" (sf-log-bool (= *sf-by-layer* "1")))
                   )
                 )
                 (setq res      *sf-dlg-result*
                       keepLoop nil
                 )
                )
                (T
                 (sf-log-event "INFO" "DIALOG_CLOSE"
                   (list
                     (cons "rc"       rc)
                     (cons "outcome"  "cancel")
                     (cons "by_layer" (sf-log-bool (= *sf-by-layer* "1")))
                   )
                 )
                 (setq res      nil
                       keepLoop nil
                 )
                )
              )
            )
          )
        )
      )
    )
  )

  (setq *sf-dlg-groups*      nil
        *sf-dlg-total*       nil
        *sf-dlg-layer-flags* nil
        *sf-dlg-result*      nil
  )
  res
)

(princ)
