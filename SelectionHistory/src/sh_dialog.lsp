;;; ==========================================================================
;;; src/sh_dialog.lsp - Interactive DCL Picker Dialog for SelectionHistory
;;; ==========================================================================

;; Escape backslash, double-quote, and ampersand characters for DCL strings.
(defun sh-dcl-escape (str / i len ch out)
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

;; Delete temporary DCL file if present.
(defun sh-cleanup-temp-dcl ()
  (if (and *sh-temp-dcl* (findfile *sh-temp-dcl*))
    (vl-file-delete *sh-temp-dcl*)
  )
  (setq *sh-temp-dcl* nil)
)

;; Pad a string on the right with spaces to at least width characters.
(defun sh-pad-right (str width / s)
  (setq s (if str str ""))
  (while (< (strlen s) width)
    (setq s (strcat s " "))
  )
  s
)

;; Build display metadata for all 5 slots:
;; Returns list of 5 items: (slotIdx enabled label)
(defun sh-build-slot-rows (/ rows idx histLen slot liveInfo liveCnt origCnt
                             cntStr src summary label enabled)
  (setq rows    nil
        idx     1
        histLen (length *sh-history*)
  )
  (while (<= idx *sh-max-slots*)
    (if (<= idx histLen)
      (progn
        (setq slot     (nth (1- idx) *sh-history*)
              liveInfo (sh-slot-live-info slot)
              liveCnt  (car liveInfo)
              origCnt  (cadr liveInfo)
              src      (cdr (assoc 'source slot))
              summary  (cdr (assoc 'summary slot))
              enabled  (> liveCnt 0)
        )
        (cond
          ((= liveCnt 0)
           (setq cntStr (strcat "0/" (itoa origCnt) " (erased)"))
          )
          ((< liveCnt origCnt)
           (setq cntStr (strcat (itoa liveCnt) "/" (itoa origCnt) " objs"))
          )
          ((= liveCnt 1)
           (setq cntStr "1 obj")
          )
          (T
           (setq cntStr (strcat (itoa liveCnt) " objs"))
          )
        )
        (setq label
          (strcat
            "#" (itoa idx) "  |  "
            (sh-pad-right cntStr 14) " |  "
            (sh-pad-right src 12) " |  "
            summary
          )
        )
        (setq rows (cons (list idx enabled label) rows))
      )
      (setq rows (cons (list idx nil (strcat "#" (itoa idx) "  |  (empty)")) rows))
    )
    (setq idx (1+ idx))
  )
  (reverse rows)
)

;; Write temporary DCL definition for the SelectionHistory picker dialog.
(defun sh-write-temp-dcl (/ dclPath fh idx key)
  (sh-cleanup-temp-dcl)
  (setq dclPath (vl-filename-mktemp "sh_dlg_" nil ".dcl"))
  (if (or (null dclPath) (null (setq fh (open dclPath "w"))))
    nil
    (progn
      (setq *sh-temp-dcl* dclPath)
      (write-line "selhist_dlg : dialog {" fh)
      (write-line "  label = \"Selection History (Last 5 Selections)\";" fh)
      (write-line "  : boxed_column {" fh)
      (write-line "    label = \"Saved Selections (Newest First)\";" fh)
      (write-line "    : list_box {" fh)
      (write-line "      key = \"lst_slots\";" fh)
      (write-line "      width = 72;" fh)
      (write-line "      height = 7;" fh)
      (write-line "      fixed_width_font = true;" fh)
      (write-line "    }" fh)
      (write-line "  }" fh)

      ;; Quick-pick 1..5 button row
      (write-line "  : row {" fh)
      (write-line "    : text { label = \"Quick Recall:\"; fixed_width = true; }" fh)
      (setq idx 1)
      (while (<= idx *sh-max-slots*)
        (setq key (strcat "btn_" (itoa idx)))
        (write-line
          (strcat "    : button { key = \"" key "\"; label = \"&" (itoa idx) "\"; width = 5; fixed_width = true; }")
          fh
        )
        (setq idx (1+ idx))
      )
      (write-line "    : spacer { width = 1; }" fh)
      (write-line "  }" fh)

      (write-line "  : text { label = \"Tip: Use SH1-SH5 directly, or (sh 1)-(sh 5) inside any Select objects: prompt.\"; }" fh)

      ;; Bottom OK / Cancel row
      (write-line "  : row {" fh)
      (write-line "    : spacer { width = 1; }" fh)
      (write-line "    : button { key = \"btn_select\"; label = \"&Select\"; width = 12; fixed_width = true; is_default = true; }" fh)
      (write-line "    : button { key = \"cancel\"; label = \"&Cancel\"; width = 12; fixed_width = true; is_cancel = true; }" fh)
      (write-line "  }" fh)
      (write-line "}" fh)
      (close fh)
      dclPath
    )
  )
)

;; Show the SelectionHistory DCL picker dialog.
;; Returns 1-based slot index (1..5) if user selected a valid slot, or nil.
(defun sh-show-dialog (/ rows dclPath dclId firstValid selectedIdx res row idx en)
  (setq rows       (sh-build-slot-rows)
        firstValid nil
  )
  (foreach row rows
    (if (and (null firstValid) (cadr row))
      (setq firstValid (car row))
    )
  )

  (setq dclPath (sh-write-temp-dcl))
  (if (null dclPath)
    (progn
      (princ "\nSelection History: Failed to create temporary DCL file.")
      nil
    )
    (progn
      (setq dclId (load_dialog dclPath))
      (if (or (null dclId) (< dclId 0))
        (progn
          (sh-cleanup-temp-dcl)
          (princ "\nSelection History: Failed to load dialog.")
          nil
        )
        (if (not (new_dialog "selhist_dlg" dclId))
          (progn
            (unload_dialog dclId)
            (sh-cleanup-temp-dcl)
            (princ "\nSelection History: Failed to open dialog.")
            nil
          )
          (progn
            ;; Populate list box
            (start_list "lst_slots")
            (foreach row rows
              (add_list (caddr row))
            )
            (end_list)

            ;; Enable/disable quick buttons 1..5
            (foreach row rows
              (setq idx (car row)
                    en  (cadr row)
              )
              (mode_tile (strcat "btn_" (itoa idx)) (if en 0 1))
            )

            ;; Default selection to first valid slot (or row 0 if all empty)
            (setq selectedIdx (if firstValid firstValid 1))
            (set_tile "lst_slots" (itoa (1- selectedIdx)))
            (mode_tile "btn_select" (if firstValid 0 1))

            ;; List box click / double-click handler
            (action_tile "lst_slots"
              (strcat
                "(progn "
                "  (setq selectedIdx (1+ (atoi $value))) "
                "  (if (cadr (nth (1- selectedIdx) rows)) "
                "    (progn "
                "      (mode_tile \"btn_select\" 0) "
                "      (if (= $reason 4) (done_dialog 1)) "
                "    ) "
                "    (mode_tile \"btn_select\" 1) "
                "  ) "
                ")"
              )
            )

            ;; Quick-pick button handlers (1..5)
            (foreach row rows
              (setq idx (car row))
              (action_tile (strcat "btn_" (itoa idx))
                (strcat "(setq selectedIdx " (itoa idx) ") (done_dialog 1)")
              )
            )

            (action_tile "btn_select" "(done_dialog 1)")
            (action_tile "cancel"     "(done_dialog 0)")

            (setq res (start_dialog))
            (unload_dialog dclId)
            (sh-cleanup-temp-dcl)

            (if (and (= res 1) selectedIdx (cadr (nth (1- selectedIdx) rows)))
              selectedIdx
              nil
            )
          )
        )
      )
    )
  )
)
