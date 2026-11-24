;;; ==========================================================================
;;; src/rt_math.lsp - Number Validation, Area/CHW Calculations & Formatting
;;; ==========================================================================

;; --------------------------------------------------------------------------
;; Strict numeric string validation (supports decimals & scientific notation)
;; Examples: "1e-6", "1E-2", "0.01", "-2.5", "+10", ".5", "100"
;; --------------------------------------------------------------------------
(defun rt-valid-number-p (str / s len idx ch hasDigit hasDot hasExp expDigits)
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
          ;; Optional leading sign
          (setq ch (substr s idx 1))
          (if (or (= ch "+") (= ch "-"))
            (setq idx (1+ idx))
          )
          ;; Mantissa digits and optional single decimal point
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
                     idx (1+ len)) ; Force failure on invalid char
              )
            )
          )
          ;; Optional exponent part
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
                        idx (1+ len)) ; Force failure
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
(defun rt-valid-uint-p (str / s len idx ch ok)
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
(defun rt-floor (x / f)
  (setq f (fix x))
  (if (< x f) (1- f) f)
)

(defun rt-ceil (x / f)
  (setq f (fix x))
  (if (> x f) (1+ f) f)
)

;; --------------------------------------------------------------------------
;; Rounding to specified decimal precision with mode:
;;   "nearest" -> standard half-away-from-zero
;;   "up"      -> ceiling (with floating-point epsilon guard)
;;   "down"    -> floor   (with floating-point epsilon guard)
;; --------------------------------------------------------------------------
(defun rt-round-val (val prec mode / mult scaled eps roundedInt)
  (if (< prec 0) (setq prec 0))
  (setq mult   (expt 10.0 prec)
        scaled (* (float val) mult)
        eps    1e-9
  )
  (setq roundedInt
    (cond
      ((= mode "up")
       (rt-ceil (- scaled eps))
      )
      ((= mode "down")
       (rt-floor (+ scaled eps))
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
;; Area Calculation Pipeline:
;;   1. Scale raw area by factor
;;   2. Add constant bias
;;   3. Round using selected mode & decimal precision
;; --------------------------------------------------------------------------
(defun rt-calc-area (rawArea factor bias prec roundMode / scaled biased)
  (setq scaled (* (float rawArea) (float factor))
        biased (+ scaled (float bias))
  )
  (rt-round-val biased prec roundMode)
)

;; --------------------------------------------------------------------------
;; Chilled Water Flow (GPM):
;;   GPM = (Final Area / Area Ton Factor) * 2.4
;; --------------------------------------------------------------------------
(defun rt-calc-chw (areaM2 tonFactor prec / rawGpm)
  (setq rawGpm (* (/ (float areaM2) (float tonFactor)) 2.4))
  (rt-round-val rawGpm prec "nearest")
)

;; --------------------------------------------------------------------------
;; Label Formatting (shared by MTEXT creation and Dialog live preview)
;;   sep: "\\P" for multi-line MTEXT, " | " for preview, " " for single-line
;; --------------------------------------------------------------------------
(defun rt-format-label (roomName areaM2 prec extra sep chwEnabled chwGpm / out)
  (setq out (strcat roomName sep "Area: " (rtos areaM2 2 prec) "m2"))
  (if chwEnabled
    (setq out (strcat out sep "CHW: " (rtos chwGpm 2 prec) " gpm"))
  )
  (if (and extra (/= extra ""))
    (setq out (strcat out sep extra))
  )
  out
)

(princ)
