;;; ==========================================================================
;;; src/rt_state.lsp - Persistent Session State & Room Counter Helpers
;;; ==========================================================================

;; Initialize persistent session defaults (only if not already set)
(if (not *rt-counts*)     (setq *rt-counts*     nil))
(if (not *rt-rtype*)      (setq *rt-rtype*      "Cafe"))
(if (not *rt-num*)        (setq *rt-num*        1))
(if (not *rt-extra*)      (setq *rt-extra*      ""))
(if (not *rt-height*)     (setq *rt-height*     (if (> (getvar "TEXTSIZE") 0.0) (getvar "TEXTSIZE") 250.0)))

;; Area calculation defaults
(if (not *rt-preset*)     (setq *rt-preset*     "mm"))      ; "mm", "cm", "m", or "custom"
(if (not *rt-factor*)     (setq *rt-factor*     1e-6))      ; multiplier applied to raw CAD area
(if (not *rt-factor-str*) (setq *rt-factor-str* "1e-6"))    ; display string for factor input
(if (not *rt-bias*)       (setq *rt-bias*       0.0))       ; constant offset added after scaling
(if (not *rt-round-mode*) (setq *rt-round-mode* "nearest")) ; "nearest", "up", or "down"
(if (not *rt-prec*)       (setq *rt-prec*       2))         ; decimal places

;; Chilled Water Flow (AC) defaults
(if (not *rt-chw-en*)     (setq *rt-chw-en*     "0"))       ; "1" = Enabled, "0" = Disabled
(if (not *rt-ton-factor*) (setq *rt-ton-factor* 15.0))      ; Area Ton Factor (m2/ton)

;; Output options
(if (not *rt-multiline*)  (setq *rt-multiline*  "0"))       ; "1" = Multi-line, "0" = Single-line
(if (not *rt-keepbndry*)  (setq *rt-keepbndry*  "0"))       ; "1" = Keep outer boundary, "0" = Delete

;; --------------------------------------------------------------------------
;; Counter Helpers (per room type, case-insensitive)
;; --------------------------------------------------------------------------

(defun rt-get-count (rtype / pair)
  (if (and rtype (/= rtype "") (setq pair (assoc (strcase rtype) *rt-counts*)))
    (1+ (cdr pair))
    1
  )
)

(defun rt-set-count (rtype num / key pair)
  (setq key (strcase rtype))
  (if (setq pair (assoc key *rt-counts*))
    (setq *rt-counts* (subst (cons key num) pair *rt-counts*))
    (setq *rt-counts* (cons (cons key num) *rt-counts*))
  )
)

(princ)
