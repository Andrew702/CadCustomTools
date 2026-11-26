;;; ==========================================================================
;;; src/rt_dialog.lsp - Modal Dialog Initialization, Callbacks & Validation
;;; ==========================================================================

;; Helper: find 0-based index of item in string list (case-insensitive), default 0
(defun rt-dlg-list-index (target lst / idx found targetUp)
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
(defun rt-dlg-fill-list (key items)
  (start_list key)
  (mapcar 'add_list items)
  (end_list)
)

;; Helper: read currently selected rounding mode from radio buttons
(defun rt-dlg-get-round-mode ()
  (cond
    ((= (get_tile "rnd_up")   "1") "up")
    ((= (get_tile "rnd_down") "1") "down")
    (T "nearest")
  )
)

;; Helper: read currently selected unit preset from radio buttons
(defun rt-dlg-get-preset ()
  (cond
    ((= (get_tile "p_mm") "1") "mm")
    ((= (get_tile "p_cm") "1") "cm")
    ((= (get_tile "p_m")  "1") "m")
    (T "custom")
  )
)

;; Helper: read currently selected pipe unit ("mm" or "inch")
(defun rt-dlg-get-pipe-unit ()
  (if (= (get_tile "pipe_u_in") "1") "inch" "mm")
)

;; Helper: synchronize enabled/disabled state of CHW & Pipe Sizing tiles
(defun rt-dlg-sync-pipe-modes (/ chwOn pipeOn)
  (setq chwOn  (= (get_tile "chw_en") "1")
        pipeOn (and chwOn (= (get_tile "pipe_en") "1"))
  )
  (mode_tile "tonfactor" (if chwOn  0 1))
  (mode_tile "pipe_en"   (if chwOn  0 1))
  (mode_tile "pipe_u_mm" (if pipeOn 0 1))
  (mode_tile "pipe_u_in" (if pipeOn 0 1))
  (mode_tile "min_pipe"  (if pipeOn 0 1))
)

;; Callback: preset radio button clicked
(defun rt-dlg-on-preset (preset)
  (cond
    ((= preset "mm") (set_tile "factor" "1e-6"))
    ((= preset "cm") (set_tile "factor" "1e-4"))
    ((= preset "m")  (set_tile "factor" "1"))
    ((= preset "custom") (mode_tile "factor" 2))
  )
  (rt-dlg-update-preview)
)

;; Callback: factor edit_box modified manually
(defun rt-dlg-on-factor (/ fStr)
  (setq fStr (strcase (vl-string-trim " \t" (get_tile "factor")) T))
  (cond
    ((or (= fStr "1e-6") (= fStr "0.000001")) (set_tile "p_mm" "1"))
    ((or (= fStr "1e-4") (= fStr "0.0001"))   (set_tile "p_cm" "1"))
    ((or (= fStr "1")    (= fStr "1.0"))      (set_tile "p_m"  "1"))
    (T (set_tile "p_custom" "1"))
  )
  (rt-dlg-update-preview)
)

;; Callback: Chilled Water toggle changed
(defun rt-dlg-on-chw-toggle ()
  (rt-dlg-sync-pipe-modes)
  (rt-dlg-update-preview)
)

;; Callback: Show Pipe Diameter toggle changed
(defun rt-dlg-on-pipe-toggle ()
  (rt-dlg-sync-pipe-modes)
  (rt-dlg-update-preview)
)

;; Callback: Pipe unit radio changed (refresh Min Dia labels while keeping index)
(defun rt-dlg-on-pipe-unit (/ curIdx)
  (setq curIdx (atoi (get_tile "min_pipe")))
  (rt-dlg-fill-list "min_pipe" (rt-pipe-size-labels (rt-dlg-get-pipe-unit)))
  (set_tile "min_pipe" (itoa curIdx))
  (rt-dlg-update-preview)
)

;; Callback: Keep Area Polyline toggle changed
(defun rt-dlg-on-keepbndry-toggle ()
  (mode_tile "bndry_layer" (if (= (get_tile "keepbndry") "1") 0 1))
)

;; Callback: update live label preview inside dialog
(defun rt-dlg-update-preview (/ rt sn ex prStr pr ml sep biasStr bias rmode
                                sampleArea chwEn tfStr tf chwGpm pipeEn pipeStr)
  (setq rt      (vl-string-trim " \t" (get_tile "rtype"))
        sn      (vl-string-trim " \t" (get_tile "startnum"))
        ex      (get_tile "extra")
        prStr   (vl-string-trim " \t" (get_tile "prec"))
        ml      (get_tile "multiline")
        biasStr (vl-string-trim " \t" (get_tile "bias"))
        rmode   (rt-dlg-get-round-mode)
        chwEn   (= (get_tile "chw_en") "1")
        tfStr   (vl-string-trim " \t" (get_tile "tonfactor"))
        pipeEn  (= (get_tile "pipe_en") "1")
  )
  (setq pr   (if (rt-valid-uint-p prStr) (min 6 (atoi prStr)) *rt-prec*)
        bias (if (rt-valid-number-p biasStr) (atof biasStr) 0.0)
  )
  (if (= rt "") (setq rt "Room"))
  (if (= sn "") (setq sn "1"))
  (setq sep (if (= ml "1") " | " " "))

  ;; Sample scaled area = 24.53 m2 before bias and rounding
  (setq sampleArea (rt-calc-area 24.53 1.0 bias pr rmode))

  (if (and chwEn (rt-valid-number-p tfStr) (> (atof tfStr) 0.0))
    (progn
      (setq tf     (atof tfStr)
            chwGpm (rt-calc-chw sampleArea tf pr)
      )
      (setq pipeStr
        (if pipeEn
          (rt-calc-pipe-size chwGpm (atoi (get_tile "min_pipe")) (rt-dlg-get-pipe-unit))
          nil
        )
      )
    )
    (setq chwEn   nil
          chwGpm  0.0
          pipeStr nil
    )
  )

  (set_tile "preview"
    (rt-format-label (strcat rt "#" sn) sampleArea pr ex sep chwEn chwGpm pipeStr T)
  )
)

;; Validate all dialog fields and persist to session globals
(defun rt-dlg-validate-and-save (/ rt snStr htStr fStr bStr prStr tfStr
                                   sn ht fac bias pr chwEn tf mIdx bIdx)
  (set_tile "error" "")
  (setq rt    (vl-string-trim " \t" (get_tile "rtype"))
        snStr (vl-string-trim " \t" (get_tile "startnum"))
        htStr (vl-string-trim " \t" (get_tile "height"))
        fStr  (vl-string-trim " \t" (get_tile "factor"))
        bStr  (vl-string-trim " \t" (get_tile "bias"))
        prStr (vl-string-trim " \t" (get_tile "prec"))
        chwEn (= (get_tile "chw_en") "1")
        tfStr (vl-string-trim " \t" (get_tile "tonfactor"))
  )
  (cond
    ((= rt "")
     (set_tile "error" "Please enter a Room Type.")
     (rt-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "rtype") (cons "value" rt)))
     nil
    )
    ((not (rt-valid-uint-p snStr))
     (set_tile "error" "Start Number must be a non-negative integer.")
     (rt-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "startnum") (cons "value" snStr)))
     nil
    )
    ((or (not (rt-valid-number-p htStr)) (<= (atof htStr) 0.0))
     (set_tile "error" "Font Size must be a number greater than 0.")
     (rt-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "height") (cons "value" htStr)))
     nil
    )
    ((or (not (rt-valid-number-p fStr)) (<= (atof fStr) 0.0))
     (set_tile "error" "Scale Factor must be a number greater than 0 (e.g. 1e-6 or 1e-2).")
     (rt-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "factor") (cons "value" fStr)))
     nil
    )
    ((not (rt-valid-number-p bStr))
     (set_tile "error" "Area Bias must be a valid number (positive, 0, or negative).")
     (rt-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "bias") (cons "value" bStr)))
     nil
    )
    ((or (not (rt-valid-uint-p prStr)) (> (atoi prStr) 6))
     (set_tile "error" "Decimals must be an integer between 0 and 6.")
     (rt-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "prec") (cons "value" prStr)))
     nil
    )
    ((and chwEn (or (not (rt-valid-number-p tfStr)) (<= (atof tfStr) 0.0)))
     (set_tile "error" "Area Ton Factor must be a number greater than 0.")
     (rt-log-event "WARN" "DIALOG_VALIDATION_FAIL" (list (cons "field" "tonfactor") (cons "value" tfStr)))
     nil
    )
    (T
     (setq sn   (atoi snStr)
           ht   (atof htStr)
           fac  (atof fStr)
           bias (atof bStr)
           pr   (atoi prStr)
           tf   (if (and (rt-valid-number-p tfStr) (> (atof tfStr) 0.0))
                  (atof tfStr)
                  *rt-ton-factor*)
           mIdx (atoi (get_tile "mtext_layer"))
           bIdx (atoi (get_tile "bndry_layer"))
     )
     (setq *rt-rtype*        rt
           *rt-num*          sn
           *rt-extra*        (get_tile "extra")
           *rt-height*       ht
           *rt-preset*       (rt-dlg-get-preset)
           *rt-factor*       fac
           *rt-factor-str*   fStr
           *rt-bias*         bias
           *rt-round-mode*   (rt-dlg-get-round-mode)
           *rt-prec*         pr
           *rt-chw-en*       (if chwEn "1" "0")
           *rt-ton-factor*   tf
           *rt-pipe-en*      (get_tile "pipe_en")
           *rt-pipe-unit*    (rt-dlg-get-pipe-unit)
           *rt-pipe-min-idx* (atoi (get_tile "min_pipe"))
           *rt-multiline*    (get_tile "multiline")
           *rt-keepbndry*    (get_tile "keepbndry")
           *rt-fast-bndry*   (get_tile "fastbndry")
           *rt-mtext-layer*  (if (and *rt-dlg-layers* (< mIdx (length *rt-dlg-layers*)))
                               (nth mIdx *rt-dlg-layers*)
                               "<Current>")
           *rt-bndry-layer*  (if (and *rt-dlg-layers* (< bIdx (length *rt-dlg-layers*)))
                               (nth bIdx *rt-dlg-layers*)
                               "<Current>")
     )
     T
    )
  )
)

;; Show modal dialog (returns: 1 = Pick Rooms, 2 = Export CSV, 0 = Cancel)
(defun rt-show-dialog (/ dclPath dclId rc)
  (setq dclPath (strcat *rt-root-dir* "src/rt_dialog.dcl"))
  (if (not (findfile dclPath))
    (progn
      (rt-log-event "ERROR" "DIALOG_ERROR" (list (cons "reason" "dcl_not_found") (cons "path" dclPath)))
      (princ (strcat "\nError: Dialog file not found at: " dclPath))
      0
    )
    (progn
      (setq dclId (load_dialog dclPath))
      (if (not (new_dialog "roomtag_dlg" dclId))
        (progn
          (if (> dclId 0) (unload_dialog dclId))
          (rt-log-event "ERROR" "DIALOG_ERROR" (list (cons "reason" "new_dialog_failed") (cons "path" dclPath)))
          (princ "\nError: Could not initialize HVAC Room dialog.")
          0
        )
        (progn
          ;; Build local layer list (excluding Xref layers)
          (setq *rt-dlg-layers* (rt-get-local-layers))
          (rt-dlg-fill-list "mtext_layer" *rt-dlg-layers*)
          (rt-dlg-fill-list "bndry_layer" *rt-dlg-layers*)

          ;; Populate Min Pipe Diameter dropdown
          (rt-dlg-fill-list "min_pipe" (rt-pipe-size-labels *rt-pipe-unit*))

          ;; Populate tiles from session state
          (set_tile "rtype"       *rt-rtype*)
          (set_tile "startnum"    (itoa (rt-get-count *rt-rtype*)))
          (set_tile "height"      (rtos *rt-height* 2 2))
          (set_tile "extra"       *rt-extra*)
          (set_tile "factor"      *rt-factor-str*)
          (set_tile "bias"        (rtos *rt-bias* 2 4))
          (set_tile "prec"        (itoa *rt-prec*))
          (set_tile "chw_en"      *rt-chw-en*)
          (set_tile "tonfactor"   (rtos *rt-ton-factor* 2 4))
          (set_tile "pipe_en"     *rt-pipe-en*)
          (set_tile "min_pipe"    (itoa *rt-pipe-min-idx*))
          (set_tile "multiline"   *rt-multiline*)
          (set_tile "keepbndry"   *rt-keepbndry*)
          (set_tile "fastbndry"   *rt-fast-bndry*)
          (set_tile "mtext_layer" (itoa (rt-dlg-list-index *rt-mtext-layer* *rt-dlg-layers*)))
          (set_tile "bndry_layer" (itoa (rt-dlg-list-index *rt-bndry-layer* *rt-dlg-layers*)))

          (if (= *rt-pipe-unit* "inch")
            (set_tile "pipe_u_in" "1")
            (set_tile "pipe_u_mm" "1")
          )

          (cond
            ((= *rt-preset* "mm") (set_tile "p_mm" "1"))
            ((= *rt-preset* "cm") (set_tile "p_cm" "1"))
            ((= *rt-preset* "m")  (set_tile "p_m"  "1"))
            (T                    (set_tile "p_custom" "1"))
          )
          (cond
            ((= *rt-round-mode* "up")   (set_tile "rnd_up"   "1"))
            ((= *rt-round-mode* "down") (set_tile "rnd_down" "1"))
            (T                          (set_tile "rnd_near" "1"))
          )

          ;; Initial enable/disable states
          (rt-dlg-sync-pipe-modes)
          (mode_tile "bndry_layer" (if (= *rt-keepbndry* "1") 0 1))
          (rt-dlg-update-preview)

          ;; Wire tile callbacks
          (action_tile "rtype"
            "(set_tile \"startnum\" (itoa (rt-get-count (vl-string-trim \" \\t\" $value)))) (rt-dlg-update-preview)"
          )
          (action_tile "startnum"  "(rt-dlg-update-preview)")
          (action_tile "extra"     "(rt-dlg-update-preview)")
          (action_tile "p_mm"      "(rt-dlg-on-preset \"mm\")")
          (action_tile "p_cm"      "(rt-dlg-on-preset \"cm\")")
          (action_tile "p_m"       "(rt-dlg-on-preset \"m\")")
          (action_tile "p_custom"  "(rt-dlg-on-preset \"custom\")")
          (action_tile "factor"    "(rt-dlg-on-factor)")
          (action_tile "bias"      "(rt-dlg-update-preview)")
          (action_tile "rnd_near"  "(rt-dlg-update-preview)")
          (action_tile "rnd_up"    "(rt-dlg-update-preview)")
          (action_tile "rnd_down"  "(rt-dlg-update-preview)")
          (action_tile "prec"      "(rt-dlg-update-preview)")
          (action_tile "chw_en"    "(rt-dlg-on-chw-toggle)")
          (action_tile "tonfactor" "(rt-dlg-update-preview)")
          (action_tile "pipe_en"   "(rt-dlg-on-pipe-toggle)")
          (action_tile "pipe_u_mm" "(rt-dlg-on-pipe-unit)")
          (action_tile "pipe_u_in" "(rt-dlg-on-pipe-unit)")
          (action_tile "min_pipe"  "(rt-dlg-update-preview)")
          (action_tile "multiline" "(rt-dlg-update-preview)")
          (action_tile "keepbndry" "(rt-dlg-on-keepbndry-toggle)")

          (action_tile "pick"   "(if (rt-dlg-validate-and-save) (done_dialog 1))")
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
