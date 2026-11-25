;;; ==========================================================================
;;; FireSprinkler/src/ff_math.lsp - Validation, Area, NFPA 13 Sprinklers
;;; ==========================================================================

;; --------------------------------------------------------------------------
;; Strict numeric string validation (supports decimals & scientific notation)
;; --------------------------------------------------------------------------
(defun ff-valid-number-p (str / s len idx ch hasDigit hasDot hasExp expDigits)
  (if (not (eq (type str) 'STR))
    nil
    (progn
      (setq s (vl-string-trim " \t" str)
            len (strlen s)
            idx 1
            hasDigit nil
            hasDot nil
            hasExp nil
            expDigits nil
      )
      (if (= len 0)
        nil
        (progn
          (setq ch (substr s idx 1))
          (if (or (= ch "+") (= ch "-"))
            (setq idx (1+ idx))
          )
          (while (and (<= idx len) (not hasExp))
            (setq ch (substr s idx 1))
            (cond
              ((and (>= (ascii ch) 48) (<= (ascii ch) 57))
               (setq hasDigit T
                     idx (1+ idx))
              )
              ((and (= ch ".") (not hasDot))
               (setq hasDot T
                     idx (1+ idx))
              )
              ((or (= ch "e") (= ch "E"))
               (setq hasExp T
                     idx (1+ idx))
              )
              (T
               (setq hasDigit nil
                     idx (1+ len))
              )
            )
          )
          (if (and hasDigit hasExp)
            (progn
              (if (<= idx len)
                (progn
                  (setq ch (substr s idx 1))
                  (if (or (= ch "+") (= ch "-"))
                    (setq idx (1+ idx))
                  )
                )
              )
              (while (<= idx len)
                (setq ch (substr s idx 1))
                (if (and (>= (ascii ch) 48) (<= (ascii ch) 57))
                  (setq expDigits T
                        idx (1+ idx))
                  (setq expDigits nil
                        idx (1+ len))
                )
              )
              expDigits
            )
            hasDigit
          )
        )
      )
    )
  )
)

;; --------------------------------------------------------------------------
;; Strict non-negative integer string validation
;; --------------------------------------------------------------------------
(defun ff-valid-uint-p (str / s len idx ch ok)
  (if (not (eq (type str) 'STR))
    nil
    (progn
      (setq s (vl-string-trim " \t" str)
            len (strlen s)
            idx 1
            ok (> len 0)
      )
      (while (and ok (<= idx len))
        (setq ch (ascii (substr s idx 1)))
        (if (and (>= ch 48) (<= ch 57))
          (setq idx (1+ idx))
          (setq ok nil)
        )
      )
      ok
    )
  )
)

;; --------------------------------------------------------------------------
;; Floor & Ceiling helpers (AutoLISP fix truncates toward zero)
;; --------------------------------------------------------------------------
(defun ff-floor (x / f)
  (setq f (fix x))
  (if (< x f) (1- f) f)
)

(defun ff-ceil (x / f)
  (setq f (fix x))
  (if (> x f) (1+ f) f)
)

;; --------------------------------------------------------------------------
;; Rounding to specified decimal precision ("nearest", "up", "down")
;; --------------------------------------------------------------------------
(defun ff-round-val (val prec mode / mult scaled eps roundedInt)
  (if (< prec 0) (setq prec 0))
  (setq mult   (expt 10.0 prec)
        scaled (* (float val) mult)
        eps    1e-9
  )
  (setq roundedInt
    (cond
      ((= mode "up")
       (ff-ceil (- scaled eps))
      )
      ((= mode "down")
       (ff-floor (+ scaled eps))
      )
      (T
       (if (>= scaled 0.0)
         (fix (+ scaled 0.5 eps))
         (fix (- scaled 0.5 eps))
       )
      )
    )
  )
  (/ (float roundedInt) mult)
)

;; --------------------------------------------------------------------------
;; Area Calculation Pipeline: rawArea * factor + bias, then round
;; --------------------------------------------------------------------------
(defun ff-calc-area (rawArea factor bias prec roundMode / scaled biased)
  (setq scaled (* (float rawArea) (float factor))
        biased (+ scaled (float bias))
  )
  (ff-round-val biased prec roundMode)
)

;; --------------------------------------------------------------------------
;; NFPA 13 Sprinkler Count (always integer rounded UP; >= 1 for positive area)
;;   Sprinklers = ceil(Final Area / Coverage Area per Sprinkler)
;; --------------------------------------------------------------------------
(defun ff-calc-sprinklers (areaM2 covArea / ratio)
  (if (or (<= (float areaM2) 0.0) (<= (float covArea) 0.0))
    0
    (progn
      (setq ratio (/ (float areaM2) (float covArea)))
      (max 1 (ff-ceil (- ratio 1e-9)))
    )
  )
)

;; --------------------------------------------------------------------------
;; Label Formatting (shared by MTEXT creation and Dialog live preview)
;;   sep: "\\P" for multi-line MTEXT, " | " for preview, " " for single-line
;; --------------------------------------------------------------------------
(defun ff-format-label (roomName areaM2 prec sprCount extra sep / out)
  (setq out
    (strcat roomName sep
            "Area: " (rtos areaM2 2 prec) "m2" sep
            "Sprinklers: " (itoa sprCount))
  )
  (if (and extra (/= extra ""))
    (setq out (strcat out sep extra))
  )
  out
)

(princ)
