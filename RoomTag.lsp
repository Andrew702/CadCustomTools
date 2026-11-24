;;; ==========================================================================
;;; RoomTag.lsp - Modal Dialog Room Area Tagger (Xref-compatible) & CSV Export
;;; Commands: ROOMTAG, ROOMEXPORT
;;; ==========================================================================

(vl-load-com)

;; Persistent session defaults
(if (not *rt-counts*)    (setq *rt-counts*    nil))
(if (not *rt-rtype*)     (setq *rt-rtype*     "Cafe"))
(if (not *rt-num*)       (setq *rt-num*       1))
(if (not *rt-extra*)     (setq *rt-extra*     ""))
(if (not *rt-height*)    (setq *rt-height*    (if (> (getvar "TEXTSIZE") 0.0) (getvar "TEXTSIZE") 250.0)))
(if (not *rt-unit*)      (setq *rt-unit*      "mm"))  ; "mm", "cm", or "m"
(if (not *rt-prec*)      (setq *rt-prec*      2))     ; decimal places
(if (not *rt-multiline*) (setq *rt-multiline* "0"))   ; "1" = Yes, "0" = No
(if (not *rt-keepbndry*) (setq *rt-keepbndry* "0"))   ; "1" = Yes, "0" = No

;; Helper: get area divisor based on drawing units
(defun _rt-unit-div ()
  (cond
    ((= *rt-unit* "mm") 1000000.0)
    ((= *rt-unit* "cm") 10000.0)
    (T 1.0)
  )
)

;; Helper: get/set counter per room type
(defun _rt-get-count (rtype / pair)
  (if (and rtype (/= rtype "") (setq pair (assoc (strcase rtype) *rt-counts*)))
    (1+ (cdr pair))
    1
  )
)

(defun _rt-set-count (rtype num / key pair)
  (setq key (strcase rtype))
  (if (setq pair (assoc key *rt-counts*))
    (setq *rt-counts* (subst (cons key num) pair *rt-counts*))
    (setq *rt-counts* (cons (cons key num) *rt-counts*))
  )
)

;; Helper: escape CSV field
(defun _rt-csv-escape (str)
  (if (or (vl-string-search "," str) (vl-string-search "\"" str))
    (strcat "\"" (vl-string-subst "\"\"" "\"" str) "\"")
    str
  )
)

;; Helper: write temporary DCL file
(defun _rt-write-dcl (/ path fp)
  (setq path (vl-filename-mktemp "roomtag.dcl"))
  (setq fp (open path "w"))
  (foreach line
    '(
      "roomtag_dlg : dialog {"
      "  label = \"Room Area Tagger\";"
      "  : boxed_column {"
      "    label = \"Room Info\";"
      "    : edit_box {"
      "      key = \"rtype\";"
      "      label = \"Room Type:\";"
      "      edit_width = 26;"
      "    }"
      "    : edit_box {"
      "      key = \"startnum\";"
      "      label = \"Start Number:\";"
      "      edit_width = 10;"
      "    }"
      "    : edit_box {"
      "      key = \"extra\";"
      "      label = \"Constant Text:\";"
      "      edit_width = 26;"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Format & Units\";"
      "    : radio_row {"
      "      label = \"Drawing Units:\";"
      "      : radio_button { key = \"u_mm\"; label = \"mm\"; }"
      "      : radio_button { key = \"u_cm\"; label = \"cm\"; }"
      "      : radio_button { key = \"u_m\";  label = \"m\"; }"
      "    }"
      "    : row {"
      "      : edit_box {"
      "        key = \"height\";"
      "        label = \"Text Height:\";"
      "        edit_width = 10;"
      "      }"
      "      : edit_box {"
      "        key = \"prec\";"
      "        label = \"Decimals:\";"
      "        edit_width = 6;"
      "      }"
      "    }"
      "    : row {"
      "      : toggle { key = \"multiline\"; label = \"Multi-line MTEXT\"; }"
      "      : toggle { key = \"keepbndry\"; label = \"Keep Boundary\"; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Label Preview\";"
      "    : text {"
      "      key = \"preview\";"
      "      width = 48;"
      "    }"
      "  }"
      "  errtile;"
      "  : row {"
      "    fixed_width = true;"
      "    alignment = centered;"
      "    : button {"
      "      key = \"pick\";"
      "      label = \"Pick Rooms\";"
      "      is_default = true;"
      "      width = 14;"
      "    }"
      "    : button {"
      "      key = \"export\";"
      "      label = \"Export Excel (CSV)\";"
      "      width = 18;"
      "    }"
      "    : button {"
      "      key = \"cancel\";"
      "      label = \"Cancel\";"
      "      is_cancel = true;"
      "      width = 12;"
      "    }"
      "  }"
      "}"
    )
    (write-line line fp)
  )
  (close fp)
  path
)

;; Helper: update live preview text inside dialog
(defun _rt-update-preview (/ rt sn ex pr ml sep sampleArea)
  (setq rt (vl-string-trim " " (get_tile "rtype"))
        sn (get_tile "startnum")
        ex (get_tile "extra")
        pr (atoi (get_tile "prec"))
        ml (get_tile "multiline")
  )
  (if (< pr 0) (setq pr 0))
  (if (= rt "") (setq rt "Room"))
  (if (= sn "") (setq sn "1"))
  (setq sep (if (= ml "1") " | " " "))
  (setq sampleArea (rtos 24.5 2 pr))
  (set_tile "preview"
    (strcat rt "#" sn sep "Area: " sampleArea "m2"
            (if (/= ex "") (strcat sep ex) ""))
  )
)

;; Helper: validate inputs and save to globals
(defun _rt-validate-and-save (/ rt snStr sn htStr ht prStr pr)
  (setq rt    (vl-string-trim " " (get_tile "rtype"))
        snStr (vl-string-trim " " (get_tile "startnum"))
        htStr (vl-string-trim " " (get_tile "height"))
        prStr (vl-string-trim " " (get_tile "prec"))
        sn    (atoi snStr)
        ht    (atof htStr)
        pr    (atoi prStr)
  )
  (cond
    ((= rt "")
     (set_tile "error" "Please enter a Room Type.")
     nil
    )
    ((or (= snStr "") (< sn 0))
     (set_tile "error" "Start Number must be a valid non-negative integer.")
     nil
    )
    ((<= ht 0.0)
     (set_tile "error" "Text Height must be a number greater than 0.")
     nil
    )
    ((or (= prStr "") (< pr 0))
     (set_tile "error" "Decimals must be 0 or greater.")
     nil
    )
    (T
     (setq *rt-rtype*     rt
           *rt-num*       sn
           *rt-extra*     (get_tile "extra")
           *rt-height*    ht
           *rt-prec*      pr
           *rt-multiline* (get_tile "multiline")
           *rt-keepbndry* (get_tile "keepbndry")
           *rt-unit*      (cond
                            ((= (get_tile "u_mm") "1") "mm")
                            ((= (get_tile "u_cm") "1") "cm")
                            (T "m")
                          )
     )
     T
    )
  )
)

;; Helper: show modal dialog (returns 1=Pick, 2=Export, 0=Cancel)
(defun _rt-show-dialog (/ dclPath dclId rc)
  (setq dclPath (_rt-write-dcl))
  (setq dclId (load_dialog dclPath))
  (if (not (new_dialog "roomtag_dlg" dclId))
    (progn
      (vl-file-delete dclPath)
      (princ "\nError: Could not load RoomTag dialog.")
      0
    )
    (progn
      ;; Initialize tiles
      (set_tile "rtype"     *rt-rtype*)
      (set_tile "startnum"  (itoa (_rt-get-count *rt-rtype*)))
      (set_tile "extra"     *rt-extra*)
      (set_tile "height"    (rtos *rt-height* 2 2))
      (set_tile "prec"      (itoa *rt-prec*))
      (set_tile "multiline" *rt-multiline*)
      (set_tile "keepbndry" *rt-keepbndry*)
      (cond
        ((= *rt-unit* "mm") (set_tile "u_mm" "1"))
        ((= *rt-unit* "cm") (set_tile "u_cm" "1"))
        (T                  (set_tile "u_m"  "1"))
      )
      (_rt-update-preview)

      ;; Callbacks
      (action_tile "rtype"
        "(set_tile \"startnum\" (itoa (_rt-get-count (vl-string-trim \" \" $value)))) (_rt-update-preview)"
      )
      (action_tile "startnum"  "(_rt-update-preview)")
      (action_tile "extra"     "(_rt-update-preview)")
      (action_tile "prec"      "(_rt-update-preview)")
      (action_tile "multiline" "(_rt-update-preview)")
      (action_tile "pick"      "(if (_rt-validate-and-save) (done_dialog 1))")
      (action_tile "export"    "(done_dialog 2)")
      (action_tile "cancel"    "(done_dialog 0)")

      (setq rc (start_dialog))
      (unload_dialog dclId)
      (vl-file-delete dclPath)
      rc
    )
  )
)

;; Helper: detect boundary area at picked point (works with Xrefs)
(defun _rt-get-area-at (pt / lastEnt e ents maxArea maxEnt a obj)
  (setq lastEnt (entlast))
  (command "_.-boundary" "_non" pt "")
  (while (> (getvar "CMDACTIVE") 0)
    (command "")
  )
  (setq e (if lastEnt (entnext lastEnt) (entnext)))
  (while e
    (setq ents (cons e ents))
    (setq e (entnext e))
  )
  (setq maxArea nil maxEnt nil)
  (foreach ent ents
    (setq obj (vlax-ename->vla-object ent))
    (setq a (vl-catch-all-apply 'vla-get-Area (list obj)))
    (if (and (not (vl-catch-all-error-p a)) (numberp a))
      (if (or (null maxArea) (> a maxArea))
        (setq maxArea a maxEnt ent)
      )
    )
  )
  ;; Delete temp boundaries (keep outer boundary if option is checked)
  (foreach ent ents
    (if (not (and (= *rt-keepbndry* "1") (equal ent maxEnt)))
      (entdel ent)
    )
  )
  maxArea
)

;; Helper: export tagged rooms to Excel-compatible CSV
(defun _rt-do-export (/ ss i e edata xd rows csvPath fp rowIdx)
  (princ "\nSelect room tags to export (or press Enter to export ALL): ")
  (setq ss (ssget '((0 . "MTEXT") (-3 ("ROOMTAG_DATA")))))
  (if (null ss)
    (setq ss (ssget "_X" '((0 . "MTEXT") (-3 ("ROOMTAG_DATA")))))
  )
  (if (null ss)
    (princ "\nNo ROOMTAG labels found in the drawing.")
    (progn
      (setq rows nil i 0)
      (repeat (sslength ss)
        (setq e     (ssname ss i)
              edata (entget e '("ROOMTAG_DATA"))
              xd    (cdr (assoc "ROOMTAG_DATA" (cdr (assoc -3 edata))))
        )
        (if xd
          (setq rows (cons (list (cdr (nth 0 xd))
                                 (cdr (nth 1 xd))
                                 (cdr (nth 2 xd)))
                           rows))
        )
        (setq i (1+ i))
      )
      (setq rows (vl-sort rows '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))
      (setq csvPath (getfiled "Export Room Areas to Excel (CSV)"
                              (strcat (getvar "DWGPREFIX") "RoomAreas.csv")
                              "csv"
                              1))
      (if csvPath
        (progn
          (setq fp (open csvPath "w"))
          (write-line "Room Name,Room Type,Area (m2)" fp)
          (setq rowIdx 1)
          (foreach r rows
            (write-line
              (strcat (_rt-csv-escape (nth 0 r)) ","
                      (_rt-csv-escape (nth 1 r)) ","
                      (rtos (nth 2 r) 2 *rt-prec*))
              fp
            )
            (setq rowIdx (1+ rowIdx))
          )
          (write-line (strcat "TOTAL,,=SUM(C2:C" (itoa rowIdx) ")") fp)
          (close fp)
          (princ (strcat "\nExported " (itoa (length rows)) " rooms to: " csvPath))
        )
      )
    )
  )
)

;; ==========================================================================
;; COMMAND: ROOMTAG
;; ==========================================================================
(defun c:ROOMTAG (/ *error* oldEcho keepLoop rc pt rawArea areaM2 roomName sep labelStr)
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (regapp "ROOMTAG_DATA")

  (setq keepLoop T)
  (while keepLoop
    (setq rc (_rt-show-dialog))
    (cond
      ;; Cancel
      ((= rc 0)
       (setq keepLoop nil)
      )
      ;; Export Excel (CSV)
      ((= rc 2)
       (_rt-do-export)
       (setq keepLoop nil)
      )
      ;; Pick Rooms
      ((= rc 1)
       (setq pt T)
       (while pt
         (initget "Dialog")
         (setq pt (getpoint (strcat "\nPick inside room for " *rt-rtype* "#" (itoa *rt-num*) " [Dialog] <Done>: ")))
         (cond
           ((= pt "Dialog")
            (setq pt nil) ; Breaks inner loop and re-opens modal dialog
           )
           ((null pt)
            (setq keepLoop nil) ; Done completely
           )
           ((listp pt)
            (setq rawArea (_rt-get-area-at pt))
            (if rawArea
              (progn
                (setq areaM2   (/ rawArea (_rt-unit-div)))
                (setq roomName (strcat *rt-rtype* "#" (itoa *rt-num*)))
                (setq sep      (if (= *rt-multiline* "1") "\\P" " "))
                (setq labelStr (strcat roomName sep "Area: " (rtos areaM2 2 *rt-prec*) "m2"
                                       (if (/= *rt-extra* "") (strcat sep *rt-extra*) "")))
                (entmake
                  (list
                    '(0 . "MTEXT")
                    '(100 . "AcDbEntity")
                    '(100 . "AcDbMText")
                    (cons 10 (trans pt 1 0))
                    (cons 40 *rt-height*)
                    '(71 . 5)
                    (cons 1 labelStr)
                    (list -3
                      (list "ROOMTAG_DATA"
                        (cons 1000 roomName)
                        (cons 1000 *rt-rtype*)
                        (cons 1040 areaM2)
                      )
                    )
                  )
                )
                (princ (strcat "\nPlaced: " roomName " -> " (rtos areaM2 2 *rt-prec*) " m2"))
                (_rt-set-count *rt-rtype* *rt-num*)
                (setq *rt-num* (1+ *rt-num*))
              )
              (princ "\nCould not detect a closed boundary. Ensure the full room is visible and has no gaps.")
            )
           )
         )
       )
      )
    )
  )

  (setvar "CMDECHO" oldEcho)
  (princ)
)

;; ==========================================================================
;; COMMAND: ROOMEXPORT (direct shortcut without opening dialog)
;; ==========================================================================
(defun c:ROOMEXPORT ()
  (_rt-do-export)
  (princ)
)

(princ "\nRoomTag.lsp loaded. Commands: ROOMTAG, ROOMEXPORT\n")
(princ)