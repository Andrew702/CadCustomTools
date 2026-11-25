;;; ==========================================================================
;;; FireSprinkler/src/ff_state.lsp - Persistent Session State & Counters
;;; ==========================================================================

;; Initialize persistent session defaults (isolated *ff-* namespace)
(if (not *ff-counts*)      (setq *ff-counts*      nil))
(if (not *ff-rtype*)       (setq *ff-rtype*       "Room"))
(if (not *ff-num*)         (setq *ff-num*         1))
(if (not *ff-extra*)       (setq *ff-extra*       ""))
(if (not *ff-height*)      (setq *ff-height*      (if (> (getvar "TEXTSIZE") 0.0) (getvar "TEXTSIZE") 250.0)))

;; Area calculation defaults
(if (not *ff-preset*)      (setq *ff-preset*      "mm"))      ; "mm", "cm", "m", or "custom"
(if (not *ff-factor*)      (setq *ff-factor*      1e-6))      ; multiplier applied to raw CAD area
(if (not *ff-factor-str*)  (setq *ff-factor-str*  "1e-6"))    ; display string for factor input
(if (not *ff-bias*)        (setq *ff-bias*        0.0))       ; constant offset added after scaling
(if (not *ff-round-mode*)  (setq *ff-round-mode*  "nearest")) ; "nearest", "up", or "down"
(if (not *ff-prec*)        (setq *ff-prec*        2))         ; decimal places

;; NFPA 13 Sprinkler Hazard defaults (exact metric: Light=18.6, Ordinary=12.1, Extra=9.3 m2)
(if (not *ff-hazard*)      (setq *ff-hazard*      "Light"))   ; "Light", "Ordinary", "Extra", or "Custom"
(if (not *ff-cov-area*)    (setq *ff-cov-area*    18.6))      ; Coverage area per sprinkler (m2)
(if (not *ff-cov-str*)     (setq *ff-cov-str*     "18.6"))    ; Display string for coverage input

;; Output & layer options
(if (not *ff-multiline*)   (setq *ff-multiline*   "0"))          ; "1" = Multi-line, "0" = Single-line
(if (not *ff-keepbndry*)   (setq *ff-keepbndry*   "0"))          ; "1" = Keep area polyline, "0" = Delete
(if (not *ff-fast-bndry*)  (setq *ff-fast-bndry*  "0"))          ; "1" = Skip island detection in -boundary
(if (not *ff-mtext-layer*) (setq *ff-mtext-layer* "<Current>"))  ; Target layer for MTEXT
(if (not *ff-bndry-layer*) (setq *ff-bndry-layer* "<Current>"))  ; Target layer for persistent area polyline

;; --------------------------------------------------------------------------
;; Counter Helpers (per room type, case-insensitive)
;; --------------------------------------------------------------------------

(defun ff-get-count (rtype / pair)
  (if (and rtype (/= rtype "") (setq pair (assoc (strcase rtype) *ff-counts*)))
    (1+ (cdr pair))
    1
  )
)

(defun ff-set-count (rtype num / key pair)
  (setq key (strcase rtype))
  (if (setq pair (assoc key *ff-counts*))
    (setq *ff-counts* (subst (cons key num) pair *ff-counts*))
    (setq *ff-counts* (cons (cons key num) *ff-counts*))
  )
)

(princ)
