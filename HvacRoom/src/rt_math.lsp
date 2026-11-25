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
;; Chilled Water Pipe Sizing Table (EDRC-STD-H-PD-001)
;;   <= 4" (100mm): Frictional loss of 4 ft of water / 100 ft
;;   >= 5" (125mm): Water velocity restricted to 8 fps (2.4 m/s)
;; Entry format: (maxGpm mmLabel inchLabel)
;; --------------------------------------------------------------------------
(setq *rt-pipe-table*
  '(
    (2.0     "15mm"  "1/2\"")
    (4.0     "20mm"  "3/4\"")
    (7.0     "25mm"  "1\"")
    (15.0    "32mm"  "1 1/4\"")
    (23.0    "40mm"  "1 1/2\"")
    (45.0    "50mm"  "2\"")
    (73.0    "65mm"  "2 1/2\"")
    (130.0   "80mm"  "3\"")
    (268.0   "100mm" "4\"")
    (499.0   "125mm" "5\"")
    (721.0   "150mm" "6\"")
    (1248.0  "200mm" "8\"")
    (1967.0  "250mm" "10\"")
    (2818.0  "300mm" "12\"")
    (3436.0  "350mm" "14\"")
    (4552.0  "400mm" "16\"")
    (5824.0  "450mm" "18\"")
    (7253.0  "500mm" "20\"")
    (10593.0 "600mm" "24\"")
    (14533.0 "700mm" "28\"")
    (16745.0 "750mm" "30\"")
  )
)

;; Return list of pipe size labels for dialog dropdown ("mm" or "inch")
(defun rt-pipe-size-labels (unit)
  (mapcar
    '(lambda (row) (if (= unit "inch") (caddr row) (cadr row)))
    *rt-pipe-table*
  )
)

;; Lookup pipe size from GPM using 4ft/100ft + 8fps table, enforcing minIdx
(defun rt-calc-pipe-size (gpm minIdx unit / n idx foundIdx row)
  (setq n        (length *rt-pipe-table*)
        idx      0
        foundIdx nil
  )
  (if (or (not (numberp minIdx)) (< minIdx 0))
    (setq minIdx 0)
  )
  (if (>= minIdx n)
    (setq minIdx (1- n))
  )
  (while (and (< idx n) (null foundIdx))
    (if (<= (float gpm) (car (nth idx *rt-pipe-table*)))
      (setq foundIdx idx)
      (setq idx (1+ idx))
    )
  )
  (if (null foundIdx)
    (if (= unit "inch") ">30\"" ">750mm")
    (progn
      (if (< foundIdx minIdx) (setq foundIdx minIdx))
      (setq row (nth foundIdx *rt-pipe-table*))
      (if (= unit "inch") (caddr row) (cadr row))
    )
  )
)

;; --------------------------------------------------------------------------
;; Label Formatting (shared by MTEXT creation and Dialog live preview)
;;   sep:       "\\P" for multi-line MTEXT, " | " for preview, " " for single-line
;;   pipeStr:   pipe size string (e.g. "25mm" or "1\"") or nil/""
;;   isPreview: non-nil for ASCII-safe DCL preview, nil for MTEXT (\U+03D5 phi)
;; --------------------------------------------------------------------------
(defun rt-format-label (roomName areaM2 prec extra sep chwEnabled chwGpm pipeStr isPreview / out phiSym)
  (setq out (strcat roomName sep "Area: " (rtos areaM2 2 prec) "m2"))
  (if chwEnabled
    (progn
      (setq out (strcat out sep "CHW: " (rtos chwGpm 2 prec) " gpm"))
      (if (and pipeStr (/= pipeStr ""))
        (progn
          (setq phiSym (if isPreview "phi " "\\U+03D5 "))
          (setq out (strcat out " | " phiSym pipeStr))
        )
      )
    )
  )
  (if (and extra (/= extra ""))
    (setq out (strcat out sep extra))
  )
  out
)

(princ)
