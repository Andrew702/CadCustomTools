;;; ==========================================================================
;;; src/sh_store.lsp - 5-Slot Selection History Storage & Reconstruction
;;; ==========================================================================

;; Configuration constants (preserve existing values on reload)
(if (not *sh-max-slots*)    (setq *sh-max-slots* 5))
(if (not *sh-max-entities*) (setq *sh-max-entities* 5000))

;; Per-drawing session state (preserve history across APPLOAD reloads)
(if (not *sh-history*)      (setq *sh-history* nil))
(if (not *sh-pending-pf*)   (setq *sh-pending-pf* nil))
(if (not *sh-last-prev-key*)(setq *sh-last-prev-key* nil))
(if (not *sh-suppress-key*) (setq *sh-suppress-key* nil))
(if (not *sh-last-cmd*)     (setq *sh-last-cmd* nil))

;; Retrieve the Previous selection set, falling back from "_P" to "P".
(defun sh-get-previous-ss (/ ss)
  (setq ss (vl-catch-all-apply 'ssget (list "_P")))
  (if (or (vl-catch-all-error-p ss) (null ss))
    (setq ss (vl-catch-all-apply 'ssget (list "P")))
  )
  (if (and ss (not (vl-catch-all-error-p ss)) (= (type ss) 'PICKSET) (> (sslength ss) 0))
    ss
    nil
  )
)

;; --------------------------------------------------------------------------
;; String & Entity Helpers
;; --------------------------------------------------------------------------

;; Join a list of strings with a delimiter in O(n) using 64-item chunks
;; to avoid O(n^2) strcat loops and any apply argument-count limits.
(defun sh-join-strings (lst delim / chunks cur cnt out)
  (if (null lst)
    ""
    (progn
      (setq chunks nil
            cur    nil
            cnt    0
      )
      (foreach s lst
        (if (or cur chunks)
          (setq cur (cons s (cons delim cur)))
          (setq cur (cons s cur))
        )
        (setq cnt (1+ cnt))
        (if (>= cnt 64)
          (progn
            (setq chunks (cons (apply 'strcat (reverse cur)) chunks)
                  cur    nil
                  cnt    0
            )
          )
        )
      )
      (if cur
        (setq chunks (cons (apply 'strcat (reverse cur)) chunks))
      )
      (setq out "")
      (foreach c (reverse chunks)
        (setq out (strcat out c))
      )
      out
    )
  )
)

;; Compute a canonical key string from a list of uppercase entity handles.
(defun sh-handles->key (handles)
  (if (null handles)
    ""
    (sh-join-strings (vl-sort handles '<) "|")
  )
)

;; Map DXF group 0 entity type to a compact human-readable label.
(defun sh-short-type-label (dxf0)
  (cond
    ((null dxf0)            "Unknown")
    ((= dxf0 "LINE")        "Line")
    ((= dxf0 "LWPOLYLINE")  "Polyline")
    ((= dxf0 "POLYLINE")    "Polyline")
    ((= dxf0 "CIRCLE")      "Circle")
    ((= dxf0 "ARC")         "Arc")
    ((= dxf0 "INSERT")      "Block")
    ((= dxf0 "TEXT")        "Text")
    ((= dxf0 "MTEXT")       "MText")
    ((= dxf0 "HATCH")       "Hatch")
    ((= dxf0 "DIMENSION")   "Dim")
    ((= dxf0 "LEADER")      "Leader")
    ((= dxf0 "MULTILEADER") "MLeader")
    ((= dxf0 "SPLINE")      "Spline")
    ((= dxf0 "ELLIPSE")     "Ellipse")
    ((= dxf0 "POINT")       "Point")
    ((= dxf0 "SOLID")       "Solid")
    ((= dxf0 "3DFACE")      "3DFace")
    ((= dxf0 "VIEWPORT")    "Viewport")
    ((= dxf0 "WIPEOUT")     "Wipeout")
    ((= dxf0 "IMAGE")       "Image")
    ((= dxf0 "XLINE")       "XLine")
    ((= dxf0 "RAY")         "Ray")
    (T                      dxf0)
  )
)

;; Increment count for a type label in an alist ((label . count) ...).
(defun sh-bump-type-count (label counts / hit)
  (setq hit (assoc label counts))
  (if hit
    (subst (cons label (1+ (cdr hit))) hit counts)
    (cons (cons label 1) counts)
  )
)

;; Format top 3 entity types by descending count, e.g. "6 Line, 3 Polyline, 1 Block".
(defun sh-format-type-summary (counts / sorted parts idx entry)
  (if (null counts)
    ""
    (progn
      (setq sorted (vl-sort counts
                     (function (lambda (a b)
                       (if (= (cdr a) (cdr b))
                         (< (car a) (car b))
                         (> (cdr a) (cdr b))
                       )
                     ))
                   )
            parts  nil
            idx    0
      )
      (foreach entry sorted
        (if (< idx 3)
          (setq parts (cons (strcat (itoa (cdr entry)) " " (car entry)) parts))
        )
        (setq idx (1+ idx))
      )
      (setq parts (reverse parts))
      (if (> (length sorted) 3)
        (strcat (sh-join-strings parts ", ") ", ...")
        (sh-join-strings parts ", ")
      )
    )
  )
)

;; --------------------------------------------------------------------------
;; Snapshot Capture & Ring Storage
;; --------------------------------------------------------------------------

;; Convert a <Selection set> into a lightweight snapshot alist, or nil if
;; empty or larger than *sh-max-entities*.
(defun sh-ss->snapshot (ss source / len i ent ed h typ handles counts key summary)
  (if (and ss (= (type ss) 'PICKSET))
    (progn
      (setq len (sslength ss))
      (if (and (> len 0) (<= len *sh-max-entities*))
        (progn
          (setq i       0
                handles nil
                counts  nil
          )
          (while (< i len)
            (setq ent (ssname ss i)
                  ed  (if ent (entget ent) nil)
                  h   (if ed (cdr (assoc 5 ed)) nil)
                  typ (if ed (sh-short-type-label (cdr (assoc 0 ed))) nil)
            )
            (if (and h (= (type h) 'STR) (> (strlen h) 0))
              (progn
                (setq handles (cons (strcase h) handles))
                (if typ
                  (setq counts (sh-bump-type-count typ counts))
                )
              )
            )
            (setq i (1+ i))
          )
          (if handles
            (progn
              (setq handles (reverse handles)
                    key     (sh-handles->key handles)
                    summary (sh-format-type-summary counts)
              )
              (list
                (cons 'handles handles)
                (cons 'key     key)
                (cons 'source  (if (and source (> (strlen source) 0)) source "Selection"))
                (cons 'summary summary)
                (cons 'count   (length handles))
              )
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

;; Trim a list to at most n elements.
(defun sh-take-n (lst n / out)
  (setq out nil)
  (while (and lst (> n 0))
    (setq out (cons (car lst) out)
          lst (cdr lst)
          n   (1- n)
    )
  )
  (reverse out)
)

;; Push a snapshot onto *sh-history* (max *sh-max-slots*), avoiding back-to-back
;; identical sets. Returns T if added, nil if skipped.
(defun sh-push-snapshot (snap / newKey topKey)
  (if (null snap)
    nil
    (progn
      (setq newKey (cdr (assoc 'key snap))
            topKey (if *sh-history* (cdr (assoc 'key (car *sh-history*))) nil)
      )
      (if (and newKey (> (strlen newKey) 0) (not (equal newKey topKey)))
        (progn
          (setq *sh-history* (sh-take-n (cons snap *sh-history*) *sh-max-slots*))
          T
        )
        nil
      )
    )
  )
)

;; --------------------------------------------------------------------------
;; Live Validation & Recall
;; --------------------------------------------------------------------------

;; Count how many entities in a slot still exist (not erased) without
;; allocating a selection set. Returns (liveCount origCount).
(defun sh-slot-live-info (slot / handles live orig ent)
  (setq handles (cdr (assoc 'handles slot))
        orig    (cdr (assoc 'count slot))
        live    0
  )
  (foreach h handles
    (setq ent (handent h))
    (if (and ent (entget ent))
      (setq live (1+ live))
    )
  )
  (list live orig)
)

;; Reconstruct a fresh <Selection set> of all live entities in a slot.
;; Returns (ss liveHandles liveKey liveCount origCount).
(defun sh-build-live-ss (slot / handles orig ss liveHandles ent liveKey)
  (setq handles     (cdr (assoc 'handles slot))
        orig        (cdr (assoc 'count slot))
        ss          (ssadd)
        liveHandles nil
  )
  (foreach h handles
    (setq ent (handent h))
    (if (and ent (entget ent))
      (progn
        (ssadd ent ss)
        (setq liveHandles (cons h liveHandles))
      )
    )
  )
  (setq liveHandles (reverse liveHandles)
        liveKey     (sh-handles->key liveHandles)
  )
  (list ss liveHandles liveKey (length liveHandles) orig)
)

;; Recall slot idx (1-based, 1..*sh-max-slots*).
;; If forCommand is T (transparent mid-command call), returns the <Selection set>
;; (or nil). If forCommand is nil, applies pickfirst grips via (sssetfirst nil ss).
;; Always preserves *sh-history* order via *sh-suppress-key*.
(defun sh-recall-slot (idx forCommand / histLen slot built ss liveKey liveCnt origCnt erasedCnt src)
  (setq histLen (length *sh-history*))
  (cond
    ((or (not (numberp idx)) (< idx 1) (> idx *sh-max-slots*))
     (princ (strcat "\nSelection History: Slot must be between 1 and " (itoa *sh-max-slots*) "."))
     nil
    )
    ((= histLen 0)
     (princ "\nSelection History: No selections recorded yet.")
     nil
    )
    ((> idx histLen)
     (princ (strcat "\nSelection History: Slot #" (itoa idx) " is empty (only "
                    (itoa histLen) (if (= histLen 1) " slot" " slots") " recorded)."))
     nil
    )
    (T
     (setq slot      (nth (1- idx) *sh-history*)
           built     (sh-build-live-ss slot)
           ss        (nth 0 built)
           liveKey   (nth 2 built)
           liveCnt   (nth 3 built)
           origCnt   (nth 4 built)
           erasedCnt (- origCnt liveCnt)
           src       (cdr (assoc 'source slot))
     )
     (if (= liveCnt 0)
       (progn
         (princ (strcat "\nSelection History #" (itoa idx)
                        ": All " (itoa origCnt) " objects have been erased."))
         nil
       )
       (progn
         ;; Guard against re-recording or reordering on recall
         (setq *sh-suppress-key* liveKey
               *sh-pending-pf*   nil
         )
         (if forCommand
           (progn
             (if (> erasedCnt 0)
               (princ (strcat "\n[SH #" (itoa idx) ": " (itoa liveCnt) " of "
                              (itoa origCnt) " objects, " (itoa erasedCnt) " erased] "))
               (princ (strcat "\n[SH #" (itoa idx) ": " (itoa liveCnt)
                              (if (= liveCnt 1) " object" " objects") "] "))
             )
             ss
           )
           (progn
             (sssetfirst nil ss)
             (if (> erasedCnt 0)
               (princ (strcat "\nSelection History #" (itoa idx) " [" src "]: Selected "
                              (itoa liveCnt) " of " (itoa origCnt) " objects ("
                              (itoa erasedCnt) " erased)."))
               (princ (strcat "\nSelection History #" (itoa idx) " [" src "]: Selected "
                              (itoa liveCnt) (if (= liveCnt 1) " object." " objects.")))
             )
             (if (= (getvar "PICKFIRST") 0)
               (princ "\nNote: Set PICKFIRST to 1 to keep grips active after recall.")
             )
             T
           )
         )
       )
     )
    )
  )
)
