;;; ==========================================================================
;;; src/rt_export.lsp - Excel-Compatible CSV Export with Optional CHW Column
;;; ==========================================================================

;; Escape CSV field per RFC-4180
(defun rt-csv-escape (str)
  (if (or (vl-string-search "," str) (vl-string-search "\"" str))
    (strcat "\"" (vl-string-subst "\"\"" "\"" str) "\"")
    str
  )
)

;; Parse a single MTEXT entity's ROOMTAG_DATA XDATA into a row list:
;;   (roomName rtype areaM2 chwEnabled chwGpm)
;; Backward-compatible with v1 tags that only stored 3 XDATA items.
(defun rt-parse-xdata (xd / name rtype area chwEn chwGpm)
  (if (and xd (>= (length xd) 3))
    (progn
      (setq name  (cdr (nth 0 xd))
            rtype (cdr (nth 1 xd))
            area  (cdr (nth 2 xd))
      )
      (if (>= (length xd) 5)
        (setq chwEn  (= (cdr (nth 3 xd)) 1)
              chwGpm (cdr (nth 4 xd))
        )
        (setq chwEn  nil
              chwGpm 0.0
        )
      )
      (list name rtype area chwEn chwGpm)
    )
    nil
  )
)

;; Build a natural sort key from a room name like "Cafe#12":
;;   (upperPrefix hasNum numVal upperFullName)
;; Splits on the last "#" so "Cafe#2" sorts before "Cafe#10".
(defun rt-room-sort-key (name / up pos prefix numStr)
  (setq up  (strcase name)
        pos (vl-string-position (ascii "#") up nil T)
  )
  (if pos
    (progn
      (setq prefix (if (> pos 0) (substr up 1 pos) "")
            numStr (substr up (+ pos 2))
      )
      (if (rt-valid-uint-p numStr)
        (list prefix T (atoi numStr) up)
        (list up nil 0 up)
      )
    )
    (list up nil 0 up)
  )
)

;; Natural comparator for exported room rows (sorts by room type, then numeric #)
(defun rt-compare-room-rows (a b / ka kb)
  (setq ka (rt-room-sort-key (car a))
        kb (rt-room-sort-key (car b))
  )
  (cond
    ((< (nth 0 ka) (nth 0 kb)) T)
    ((> (nth 0 ka) (nth 0 kb)) nil)
    ((and (nth 1 ka) (nth 1 kb)) (< (nth 2 ka) (nth 2 kb)))
    (T (< (nth 3 ka) (nth 3 kb)))
  )
)

;; Export tagged rooms to Excel-compatible CSV
(defun rt-do-export (/ ss i e edata xd row rows hasChw csvPath fp rowIdx line)
  (princ "\nSelect room tags to export (or press Enter to export ALL): ")
  (setq ss (ssget '((0 . "MTEXT") (-3 ("ROOMTAG_DATA")))))
  (if (null ss)
    (setq ss (ssget "_X" '((0 . "MTEXT") (-3 ("ROOMTAG_DATA")))))
  )
  (if (null ss)
    (princ "\nNo ROOMTAG labels found in the drawing.")
    (progn
      (setq rows   nil
            hasChw (= *rt-chw-en* "1")
            i      0
      )
      (repeat (sslength ss)
        (setq e     (ssname ss i)
              edata (entget e '("ROOMTAG_DATA"))
              xd    (cdr (assoc "ROOMTAG_DATA" (cdr (assoc -3 edata))))
              row   (rt-parse-xdata xd)
        )
        (if row
          (progn
            (if (nth 3 row) (setq hasChw T))
            (setq rows (cons row rows))
          )
        )
        (setq i (1+ i))
      )
      (setq rows (vl-sort rows 'rt-compare-room-rows))
      (setq csvPath (getfiled "Export Room Areas to Excel (CSV)"
                              (strcat (getvar "DWGPREFIX") "RoomAreas.csv")
                              "csv"
                              1))
      (if csvPath
        (progn
          (setq fp (open csvPath "w"))
          (if hasChw
            (write-line "Room Name,Room Type,Area (m2),CHW Flow (gpm)" fp)
            (write-line "Room Name,Room Type,Area (m2)" fp)
          )
          (setq rowIdx 1)
          (foreach r rows
            (setq line
              (strcat (rt-csv-escape (nth 0 r)) ","
                      (rt-csv-escape (nth 1 r)) ","
                      (rtos (nth 2 r) 2 *rt-prec*))
            )
            (if hasChw
              (setq line
                (strcat line ","
                        (if (nth 3 r)
                          (rtos (nth 4 r) 2 *rt-prec*)
                          ""
                        ))
              )
            )
            (write-line line fp)
            (setq rowIdx (1+ rowIdx))
          )
          (if hasChw
            (write-line
              (strcat "TOTAL,,=SUM(C2:C" (itoa rowIdx) "),=SUM(D2:D" (itoa rowIdx) ")")
              fp
            )
            (write-line
              (strcat "TOTAL,,=SUM(C2:C" (itoa rowIdx) ")")
              fp
            )
          )
          (close fp)
          (princ (strcat "\nExported " (itoa (length rows)) " rooms to: " csvPath))
        )
      )
    )
  )
)

(princ)
