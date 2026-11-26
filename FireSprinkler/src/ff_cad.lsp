;;; ==========================================================================
;;; FireSprinkler/src/ff_cad.lsp - Layers, Boundary Preview & MTEXT/XDATA
;;; ==========================================================================

;; Return sorted list of local drawing layer names (excludes Xref "|" layers),
;; prefixed with "<Current>" so the user can always follow CLAYER.
(defun ff-get-local-layers (/ lay name names)
  (setq lay (tblnext "LAYER" T))
  (while lay
    (setq name (cdr (assoc 2 lay)))
    (if (and name (not (vl-string-search "|" name)))
      (setq names (cons name names))
    )
    (setq lay (tblnext "LAYER"))
  )
  (cons "<Current>" (vl-sort names '(lambda (a b) (< (strcase a) (strcase b)))))
)

;; Resolve a layer choice ("<Current>" or explicit name) to an actual layer name
(defun ff-resolve-layer (layerChoice)
  (if (and layerChoice
           (/= layerChoice "")
           (/= layerChoice "<Current>")
           (tblsearch "LAYER" layerChoice))
    layerChoice
    nil
  )
)

;; Detect closed boundary at picked point (Xref-compatible).
;; When skipIslands is non-nil, disables Island Detection (Nearest ray cast)
;; for faster boundary tracing on dense floorplans.
;; Deletes inner island boundaries and returns (list maxArea maxEnt).
(defun ff-get-area-at (pt skipIslands / lastEnt e ents maxArea maxEnt a obj)
  (setq lastEnt (entlast))
  (if skipIslands
    (command "_.-boundary" "_A" "_I" "_N" "_N" "" "_non" pt "")
    (command "_.-boundary" "_non" pt "")
  )
  (while (> (getvar "CMDACTIVE") 0)
    (command "")
  )
  (setq e (if lastEnt (entnext lastEnt) (entnext)))
  (while e
    (setq ents (cons e ents))
    (setq e (entnext e))
  )
  (setq maxArea nil
        maxEnt  nil
  )
  (foreach ent ents
    (setq obj (vlax-ename->vla-object ent))
    (setq a (vl-catch-all-apply 'vla-get-Area (list obj)))
    (if (and (not (vl-catch-all-error-p a)) (numberp a))
      (if (or (null maxArea) (> a maxArea))
        (setq maxArea a
              maxEnt  ent
        )
      )
    )
  )
  (foreach ent ents
    (if (not (equal ent maxEnt))
      (entdel ent)
    )
  )
  (if (and maxArea maxEnt)
    (list maxArea maxEnt)
    nil
  )
)

;; Instantly read area and bounding-box center (in UCS) from an existing
;; closed polyline, circle, ellipse, region, or hatch. Returns (list area pt) or nil.
(defun ff-get-area-from-ent (ent / obj a minPt maxPt p1 p2 mid)
  (if (and ent (entget ent))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq a (vl-catch-all-apply 'vla-get-Area (list obj)))
      (if (and (not (vl-catch-all-error-p a)) (numberp a) (> a 0.0))
        (progn
          (if (not (vl-catch-all-error-p
                     (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minPt 'maxPt))))
            (progn
              (setq p1  (vlax-safearray->list minPt)
                    p2  (vlax-safearray->list maxPt)
                    mid (list (/ (+ (car p1) (car p2)) 2.0)
                              (/ (+ (cadr p1) (cadr p2)) 2.0)
                              0.0)
              )
              (list a (trans mid 0 1))
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

;; Highlight the detected boundary polyline in bright green as a quick preview
(defun ff-show-preview (ent / obj)
  (if (and ent (entget ent))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (vl-catch-all-apply 'vla-put-Color (list obj 3)) ; ACI 3 = Green
      (vl-catch-all-apply 'vla-Update (list obj))
      (redraw ent 3) ; Highlight
    )
  )
)

;; Clear the preview polyline:
;; - If owned is nil (pre-existing selected object): only unhighlight it.
;; - If owned is non-nil and keepBoundary is non-nil: set targetLayer, ByLayer, unhighlight.
;; - If owned is non-nil and keepBoundary is nil: delete temporary boundary.
(defun ff-clear-preview (ent keepBoundary bndryLayer owned / obj resolvedLayer)
  (if (and ent (entget ent))
    (cond
      ((not owned)
       (redraw ent 4) ; Unhighlight existing object only
      )
      (keepBoundary
       (setq obj           (vlax-ename->vla-object ent)
             resolvedLayer (ff-resolve-layer bndryLayer)
       )
       (if resolvedLayer
         (vl-catch-all-apply 'vla-put-Layer (list obj resolvedLayer))
       )
       (vl-catch-all-apply 'vla-put-Color (list obj 256)) ; ByLayer
       (vl-catch-all-apply 'vla-Update (list obj))
       (redraw ent 4) ; Unhighlight
      )
      (T
       (entdel ent)
      )
    )
  )
)

;; Create centered MTEXT entity with FFSPRINKLER_DATA XDATA attached.
;; XDATA layout:
;;   0: (1000 . roomName)      - e.g. "Office#1"
;;   1: (1000 . rtype)         - e.g. "Office"
;;   2: (1040 . areaM2)        - final adjusted & rounded area (m2)
;;   3: (1000 . hazardLabel)   - "Light", "Ordinary", "Extra", or "Custom"
;;   4: (1040 . covArea)       - coverage area per sprinkler (m2)
;;   5: (1070 . sprCount)      - integer sprinkler count (rounded up)
(defun ff-create-room-mtext (pt height labelStr roomName rtype areaM2
                             hazardLabel covArea sprCount mtextLayer
                             / resolvedLayer dxf)
  (regapp "FFSPRINKLER_DATA")
  (setq resolvedLayer (ff-resolve-layer mtextLayer))
  (setq dxf
    (append
      (list
        '(0 . "MTEXT")
        '(100 . "AcDbEntity")
      )
      (if resolvedLayer
        (list (cons 8 resolvedLayer))
        nil
      )
      (list
        '(100 . "AcDbMText")
        (cons 10 (trans pt 1 0))
        (cons 40 (float height))
        '(71 . 5) ; Middle-Center attachment
        (cons 1 labelStr)
        (list -3
          (list "FFSPRINKLER_DATA"
            (cons 1000 roomName)
            (cons 1000 rtype)
            (cons 1040 (float areaM2))
            (cons 1000 hazardLabel)
            (cons 1040 (float covArea))
            (cons 1070 sprCount)
          )
        )
      )
    )
  )
  (if (entmake dxf)
    (entlast)
    nil
  )
)

;; Read (list roomName rtype areaM2 source) from an existing MTEXT entity:
;;   1. FFSPRINKLER_DATA XDATA
;;   2. ROOMTAG_DATA XDATA (from HvacRoom / RoomTag)
;;   3. Fallback: parse "Area: <num>" from MTEXT text string
(defun ff-read-tag-area-info (ent txt / edata xdList xd areaM2 roomName rtype firstTok spPos)
  (setq edata  (entget ent '("FFSPRINKLER_DATA" "ROOMTAG_DATA"))
        xdList (cdr (assoc -3 edata))
  )
  (cond
    ((and (setq xd (cdr (assoc "FFSPRINKLER_DATA" xdList)))
          (>= (length xd) 3)
          (numberp (cdr (nth 2 xd)))
          (> (cdr (nth 2 xd)) 0.0))
     (list (cdr (nth 0 xd)) (cdr (nth 1 xd)) (cdr (nth 2 xd)) "FFSPRINKLER_DATA")
    )
    ((and (setq xd (cdr (assoc "ROOMTAG_DATA" xdList)))
          (>= (length xd) 3)
          (numberp (cdr (nth 2 xd)))
          (> (cdr (nth 2 xd)) 0.0))
     (list (cdr (nth 0 xd)) (cdr (nth 1 xd)) (cdr (nth 2 xd)) "ROOMTAG_DATA")
    )
    ((setq areaM2 (ff-extract-area-from-text txt))
     ;; Derive a best-effort room name from the first token before space / \P
     (setq firstTok (vl-string-trim " \t" txt))
     (if (setq spPos (vl-string-search "\\P" firstTok))
       (setq firstTok (substr firstTok 1 spPos))
     )
     (if (setq spPos (vl-string-search " " firstTok))
       (setq firstTok (substr firstTok 1 spPos))
     )
     (setq roomName (if (/= firstTok "") firstTok "Room")
           rtype    (if (setq spPos (vl-string-position (ascii "#") roomName nil T))
                      (if (> spPos 0) (substr roomName 1 spPos) roomName)
                      roomName)
     )
     (list roomName rtype areaM2 "TEXT_FALLBACK")
    )
    (T nil)
  )
)

;; Attach or update FFSPRINKLER_DATA XDATA on an existing entity while
;; preserving any other registered XDATA (e.g. ROOMTAG_DATA from HvacRoom).
(defun ff-attach-xdata (ent roomName rtype areaM2 hazardLabel covArea sprCount
                        / edata oldXd otherXd newApp newXd)
  (regapp "FFSPRINKLER_DATA")
  (setq edata (entget ent '("*"))
        oldXd (cdr (assoc -3 edata))
        otherXd (vl-remove-if '(lambda (x) (= (strcase (car x)) "FFSPRINKLER_DATA")) oldXd)
        newApp  (list "FFSPRINKLER_DATA"
                  (cons 1000 roomName)
                  (cons 1000 rtype)
                  (cons 1040 (float areaM2))
                  (cons 1000 hazardLabel)
                  (cons 1040 (float covArea))
                  (cons 1070 sprCount)
                )
        newXd   (cons -3 (append otherXd (list newApp)))
  )
  (if (assoc -3 edata)
    (entmod (subst newXd (assoc -3 edata) edata))
    (entmod (append edata (list newXd)))
  )
)

;; Prompt user to select multiple existing MTEXT area tags and append
;; (or update in-place) "Sprinklers: <N>" + FFSPRINKLER_DATA XDATA.
(defun ff-append-sprinklers-to-selection (/ ss selCount i ent obj txt info areaM2
                                            sprCount newTxt updated skipped)
  (princ
    (strcat "\nSelect MTEXT area tags to append sprinklers ("
            *ff-hazard* ", " *ff-cov-str* " m2/sprinkler): ")
  )
  (setq ss (ssget '((0 . "MTEXT"))))
  (if (null ss)
    (progn
      (ff-log-event "INFO" "APPEND_COMPLETE"
        (list
          (cons "selected_count" 0)
          (cons "updated"        0)
          (cons "skipped"        0)
          (cons "hazard"         *ff-hazard*)
          (cons "cov_area"       (float *ff-cov-area*))
        )
      )
      (princ "\nNo MTEXT tags selected.")
    )
    (progn
      (setq selCount (sslength ss)
            i        0
            updated  0
            skipped  0
      )
      (repeat selCount
        (setq ent  (ssname ss i)
              obj  (vlax-ename->vla-object ent)
              txt  (vl-catch-all-apply 'vla-get-TextString (list obj))
        )
        (if (and (not (vl-catch-all-error-p txt))
                 (setq info (ff-read-tag-area-info ent txt)))
          (progn
            (setq areaM2   (nth 2 info)
                  sprCount (ff-calc-sprinklers areaM2 *ff-cov-area*)
                  newTxt   (ff-append-or-replace-sprinklers txt sprCount (= *ff-multiline* "1"))
            )
            (vl-catch-all-apply 'vla-put-TextString (list obj newTxt))
            (ff-attach-xdata
              ent (nth 0 info) (nth 1 info) areaM2 *ff-hazard* *ff-cov-area* sprCount
            )
            (setq updated (1+ updated))
            (ff-log-event "INFO" "APPEND_ITEM"
              (list
                (cons "handle"     (ff-log-handle ent))
                (cons "room_name"  (nth 0 info))
                (cons "source"     (nth 3 info))
                (cons "area_m2"    (float areaM2))
                (cons "cov_area"   (float *ff-cov-area*))
                (cons "sprinklers" sprCount)
                (cons "updated"    'true)
              )
            )
          )
          (progn
            (setq skipped (1+ skipped))
            (ff-log-event "WARN" "APPEND_ITEM"
              (list
                (cons "handle"  (ff-log-handle ent))
                (cons "source"  "NONE")
                (cons "updated" 'false)
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (ff-log-event "INFO" "APPEND_COMPLETE"
        (list
          (cons "selected_count" selCount)
          (cons "updated"        updated)
          (cons "skipped"        skipped)
          (cons "hazard"         *ff-hazard*)
          (cons "cov_area"       (float *ff-cov-area*))
        )
      )
      (princ
        (strcat "\nUpdated " (itoa updated) " tag(s) with sprinkler counts"
                (if (> skipped 0)
                  (strcat " (skipped " (itoa skipped) " without area data).")
                  "."
                ))
      )
    )
  )
)

(princ)
