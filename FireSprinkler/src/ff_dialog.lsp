;;; ==========================================================================
;;; FireSprinkler/src/ff_dialog.lsp - Dialog Initialization, Callbacks & Check
;;; ==========================================================================

;; Helper: find 0-based index of item in string list (case-insensitive), default 0
(defun ff-dlg-list-index (target lst / idx found targetUp)
  (setq idx      0
        found    nil
        targetUp (strcase target)
  )
  (while (and lst (not found))
    (if (= (strcase (car lst)) targetUp)
      (setq found idx)
      (setq idx (1+ idx)
            lst (cdr lst)
      )
    )
  )
  (if found found 0)
)

;; Helper: populate a DCL popup_list tile with a list of strings
(defun ff-dlg-fill-list (key items)
  (start_list key)
  (mapcar 'add_list items)
  (end_list)
)

;; Helper: read currently selected rounding mode
(defun ff-dlg-get-round-mode ()
  (cond
    ((= (get_tile "rnd_up")   "1") "up")
    ((= (get_tile "rnd_down") "1") "down")
    (T "nearest")
  )
)

;; Helper: read currently selected unit preset
(defun ff-dlg-get-preset ()
  (cond
    ((= (get_tile "p_mm") "1") "mm")
    ((= (get_tile "p_cm") "1") "cm")
    ((= (get_tile "p_m")  "1") "m")
    (T "custom")
  )
)

;; Helper: read currently selected NFPA 13 hazard class
(defun ff-dlg-get-hazard ()
  (cond
    ((= (get_tile "h_light") "1") "Light")
    ((= (get_tile "h_ord")   "1") "Ordinary")
    ((= (get_tile "h_extra") "1") "Extra")
    (T "Custom")
  )
)

;; Callback: unit preset radio clicked
(defun ff-dlg-on-preset (preset)
  (cond
    ((= preset "mm") (set_tile "factor" "1e-6"))
    ((= preset "cm") (set_tile "factor" "1e-4"))
    ((= preset "m")  (set_tile "factor" "1"))
    ((= preset "custom") (mode_tile "factor" 2))
  )
  (ff-dlg-update-preview)
)

;; Callback: factor edit_box modified manually
(defun ff-dlg-on-factor (/ fStr)
  (setq fStr (strcase (vl-string-trim " \t" (get_tile "factor")) T))
  (cond
    ((or (= fStr "1e-6") (= fStr "0.000001")) (set_tile "p_mm" "1"))
    ((or (= fStr "1e-4") (= fStr "0.0001"))   (set_tile "p_cm" "1"))
    ((or (= fStr "1")    (= fStr "1.0"))      (set_tile "p_m"  "1"))
    (T (set_tile "p_custom" "1"))
  )
  (ff-dlg-update-preview)
)

;; Callback: NFPA 13 hazard class radio clicked
(defun ff-dlg-on-hazard (haz)
  (cond
    ((= haz "Light")    (set_tile "cov_area" "18.6"))
    ((= haz "Ordinary") (set_tile "cov_area" "12.1"))
    ((= haz "Extra")    (set_tile "cov_area" "9.3"))
    ((= haz "Custom")   (mode_tile "cov_area" 2))
  )
  (ff-dlg-update-preview)
)

;; Callback: coverage area edit_box modified manually
(defun ff-dlg-on-cov (/ cStr cVal)
  (setq cStr (vl-string-trim " \t" (get_tile "cov_area")))
  (if (ff-valid-number-p cStr)
    (progn
      (setq cVal (atof cStr))
      (cond
        ((equal cVal 18.6 1e-6) (set_tile "h_light" "1"))
        ((equal cVal 12.1 1e-6) (set_tile "h_ord"   "1"))
        ((equal cVal 9.3  1e-6) (set_tile "h_extra" "1"))
        (T                      (set_tile "h_custom" "1"))
      )
    )
    (set_tile "h_custom" "1")
  )
  (ff-dlg-update-preview)
)

;; Callback: Keep Area Polyline toggle changed
(defun ff-dlg-on-keepbndry-toggle ()
  (mode_tile "bndry_layer" (if (= (get_tile "keepbndry") "1") 0 1))
)

;; Callback: update live label preview inside dialog
(defun ff-dlg-update-preview (/ rt sn ex prStr pr ml sep biasStr bias rmode
                                covStr cov sampleArea sprCount)
  (setq rt      (vl-string-trim " \t" (get_tile "rtype"))
        sn      (vl-string-trim " \t" (get_tile "startnum"))
        ex      (get_tile "extra")
        prStr   (vl-string-trim " \t" (get_tile "prec"))
        ml      (get_tile "multiline")
        biasStr (vl-string-trim " \t" (get_tile "bias"))
        rmode   (ff-dlg-get-round-mode)
        covStr  (vl-string-trim " \t" (get_tile "cov_area"))
  )
  (setq pr   (if (ff-valid-uint-p prStr) (min 6 (atoi prStr)) *ff-prec*)
        bias (if (ff-valid-number-p biasStr) (atof biasStr) 0.0)
        cov  (if (and (ff-valid-number-p covStr) (> (atof covStr) 0.0))
               (atof covStr)
               *ff-cov-area*)
  )
  (if (= rt "") (setq rt "Room"))
  (if (= sn "") (setq sn "1"))
  (setq sep (if (= ml "1") " | " " "))

  ;; Sample scaled area = 24.53 m2 before bias and rounding
  (setq sampleArea (ff-calc-area 24.53 1.0 bias pr rmode)
        sprCount   (ff-calc-sprinklers sampleArea cov)
  )

  (set_tile "preview"
    (ff-format-label (strcat rt "#" sn) sampleArea pr sprCount ex sep)
  )
)

;; Validate all dialog fields and persist to session globals
(defun ff-dlg-validate-and-save (/ rt snStr htStr fStr bStr prStr covStr
                                   sn ht fac bias pr cov mIdx bIdx)
  (set_tile "error" "")
  (setq rt     (vl-string-trim " \t" (get_tile "rtype"))
        snStr  (vl-string-trim " \t" (get_tile "startnum"))
        htStr  (vl-string-trim " \t" (get_tile "height"))
        fStr   (vl-string-trim " \t" (get_tile "factor"))
        bStr   (vl-string-trim " \t" (get_tile "bias"))
        prStr  (vl-string-trim " \t" (get_tile "prec"))
        covStr (vl-string-trim " \t" (get_tile "cov_area"))
  )
  (cond
    ((= rt "")
     (set_tile "error" "Please enter a Room Type.")
     (ff-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "rtype") (cons "value" rt)))
     nil
    )
    ((not (ff-valid-uint-p snStr))
     (set_tile "error" "Start Number must be a non-negative integer.")
     (ff-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "startnum") (cons "value" snStr)))
     nil
    )
    ((or (not (ff-valid-number-p htStr)) (<= (atof htStr) 0.0))
     (set_tile "error" "Font Size must be a number greater than 0.")
     (ff-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "height") (cons "value" htStr)))
     nil
    )
    ((or (not (ff-valid-number-p fStr)) (<= (atof fStr) 0.0))
     (set_tile "error" "Scale Factor must be a number greater than 0 (e.g. 1e-6 or 1e-2).")
     (ff-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "factor") (cons "value" fStr)))
     nil
    )
    ((not (ff-valid-number-p bStr))
     (set_tile "error" "Area Bias must be a valid number (positive, 0, or negative).")
     (ff-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "bias") (cons "value" bStr)))
     nil
    )
    ((or (not (ff-valid-uint-p prStr)) (> (atoi prStr) 6))
     (set_tile "error" "Decimals must be an integer between 0 and 6.")
     (ff-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "prec") (cons "value" prStr)))
     nil
    )
    ((or (not (ff-valid-number-p covStr)) (<= (atof covStr) 0.0))
     (set_tile "error" "Area per Sprinkler must be a number greater than 0.")
     (ff-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "cov_area") (cons "value" covStr)))
     nil
    )
    (T
     (setq sn   (atoi snStr)
           ht   (atof htStr)
           fac  (atof fStr)
           bias (atof bStr)
           pr   (atoi prStr)
           cov  (atof covStr)
           mIdx (atoi (get_tile "mtext_layer"))
           bIdx (atoi (get_tile "bndry_layer"))
     )
     (setq *ff-rtype*       rt
           *ff-num*         sn
           *ff-extra*       (get_tile "extra")
           *ff-height*      ht
           *ff-preset*      (ff-dlg-get-preset)
           *ff-factor*      fac
           *ff-factor-str*  fStr
           *ff-bias*        bias
           *ff-round-mode*  (ff-dlg-get-round-mode)
           *ff-prec*        pr
           *ff-hazard*      (ff-dlg-get-hazard)
           *ff-cov-area*    cov
           *ff-cov-str*     covStr
           *ff-multiline*   (get_tile "multiline")
           *ff-keepbndry*   (get_tile "keepbndry")
           *ff-fast-bndry*  (get_tile "fastbndry")
           *ff-mtext-layer* (if (and *ff-dlg-layers* (< mIdx (length *ff-dlg-layers*)))
                              (nth mIdx *ff-dlg-layers*)
                              "<Current>")
           *ff-bndry-layer* (if (and *ff-dlg-layers* (< bIdx (length *ff-dlg-layers*)))
                              (nth bIdx *ff-dlg-layers*)
                              "<Current>")
     )
     T
    )
  )
)

;; Show modal dialog (returns: 1 = Pick Rooms, 2 = Export CSV, 3 = Append, 0 = Cancel)
(defun ff-show-dialog (/ dclPath dclId rc)
  (setq dclPath (strcat *ff-root-dir* "src/ff_dialog.dcl"))
  (if (not (findfile dclPath))
    (progn
      (ff-log-event "ERROR" "DIALOG_ERROR" (list (cons "reason" "dcl_not_found") (cons "path" dclPath)))
      (princ (strcat "\nError: Dialog file not found at: " dclPath))
      0
    )
    (progn
      (setq dclId (load_dialog dclPath))
      (if (not (new_dialog "ffsprinkler_dlg" dclId))
        (progn
          (if (> dclId 0) (unload_dialog dclId))
          (ff-log-event "ERROR" "DIALOG_ERROR" (list (cons "reason" "new_dialog_failed") (cons "path" dclPath)))
          (princ "\nError: Could not initialize FireSprinkler dialog.")
          0
        )
        (progn
          ;; Build local layer list (excluding Xref layers)
          (setq *ff-dlg-layers* (ff-get-local-layers))
          (ff-dlg-fill-list "mtext_layer" *ff-dlg-layers*)
          (ff-dlg-fill-list "bndry_layer" *ff-dlg-layers*)

          ;; Populate tiles from session state
          (set_tile "rtype"       *ff-rtype*)
          (set_tile "startnum"    (itoa (ff-get-count *ff-rtype*)))
          (set_tile "height"      (rtos *ff-height* 2 2))
          (set_tile "extra"       *ff-extra*)
          (set_tile "factor"      *ff-factor-str*)
          (set_tile "bias"        (rtos *ff-bias* 2 4))
          (set_tile "prec"        (itoa *ff-prec*))
          (set_tile "cov_area"    *ff-cov-str*)
          (set_tile "multiline"   *ff-multiline*)
          (set_tile "keepbndry"   *ff-keepbndry*)
          (set_tile "fastbndry"   *ff-fast-bndry*)
          (set_tile "mtext_layer" (itoa (ff-dlg-list-index *ff-mtext-layer* *ff-dlg-layers*)))
          (set_tile "bndry_layer" (itoa (ff-dlg-list-index *ff-bndry-layer* *ff-dlg-layers*)))

          (cond
            ((= *ff-preset* "mm") (set_tile "p_mm" "1"))
            ((= *ff-preset* "cm") (set_tile "p_cm" "1"))
            ((= *ff-preset* "m")  (set_tile "p_m"  "1"))
            (T                    (set_tile "p_custom" "1"))
          )
          (cond
            ((= *ff-round-mode* "up")   (set_tile "rnd_up"   "1"))
            ((= *ff-round-mode* "down") (set_tile "rnd_down" "1"))
            (T                          (set_tile "rnd_near" "1"))
          )
          (cond
            ((= *ff-hazard* "Light")    (set_tile "h_light"  "1"))
            ((= *ff-hazard* "Ordinary") (set_tile "h_ord"    "1"))
            ((= *ff-hazard* "Extra")    (set_tile "h_extra"  "1"))
            (T                          (set_tile "h_custom" "1"))
          )

          ;; Initial enable/disable state
          (mode_tile "bndry_layer" (if (= *ff-keepbndry* "1") 0 1))
          (ff-dlg-update-preview)

          ;; Wire tile callbacks
          (action_tile "rtype"
            "(set_tile \"startnum\" (itoa (ff-get-count (vl-string-trim \" \\t\" $value)))) (ff-dlg-update-preview)"
          )
          (action_tile "startnum"  "(ff-dlg-update-preview)")
          (action_tile "extra"     "(ff-dlg-update-preview)")
          (action_tile "p_mm"      "(ff-dlg-on-preset \"mm\")")
          (action_tile "p_cm"      "(ff-dlg-on-preset \"cm\")")
          (action_tile "p_m"       "(ff-dlg-on-preset \"m\")")
          (action_tile "p_custom"  "(ff-dlg-on-preset \"custom\")")
          (action_tile "factor"    "(ff-dlg-on-factor)")
          (action_tile "bias"      "(ff-dlg-update-preview)")
          (action_tile "rnd_near"  "(ff-dlg-update-preview)")
          (action_tile "rnd_up"    "(ff-dlg-update-preview)")
          (action_tile "rnd_down"  "(ff-dlg-update-preview)")
          (action_tile "prec"      "(ff-dlg-update-preview)")
          (action_tile "h_light"   "(ff-dlg-on-hazard \"Light\")")
          (action_tile "h_ord"     "(ff-dlg-on-hazard \"Ordinary\")")
          (action_tile "h_extra"   "(ff-dlg-on-hazard \"Extra\")")
          (action_tile "h_custom"  "(ff-dlg-on-hazard \"Custom\")")
          (action_tile "cov_area"  "(ff-dlg-on-cov)")
          (action_tile "multiline" "(ff-dlg-update-preview)")
          (action_tile "keepbndry" "(ff-dlg-on-keepbndry-toggle)")

          (action_tile "pick"   "(if (ff-dlg-validate-and-save) (done_dialog 1))")
          (action_tile "append" "(if (ff-dlg-validate-and-save) (done_dialog 3))")
          (action_tile "export" "(done_dialog 2)")
          (action_tile "cancel" "(done_dialog 0)")

          (setq rc (start_dialog))
          (unload_dialog dclId)
          rc
        )
      )
    )
  )
)

(princ)
