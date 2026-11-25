;;; ==========================================================================
;;; src/rt_export.lsp - Excel-Compatible CSV Export (Area, CHW Flow, Pipe Size)
;;; ==========================================================================

;; Escape CSV field per RFC-4180 (handles commas and inch quotes like 1")
(defun rt-csv-escape (str)
  (if (or (vl-string-search "," str) (vl-string-search "\"" str))
    (strcat "\"" (vl-string-subst "\"\"" "\"" str) "\"")
    str
  )
)

;; Parse a single MTEXT entity's ROOMTAG_DATA XDATA into a row list:
;;   (roomName rtype areaM2 chwEnabled chwGpm pipeEnabled pipeSize)
;; Backward-compatible with v1 (3-item) and v1.1 (6-item) tags.
(defun rt-parse-xdata (xd / name rtype area chwEn chwGpm pipeEn pipeSize)
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
      (if (>= (length xd) 8)
        (setq pipeEn   (= (cdr (nth 6 xd)) 1)
              pipeSize (cdr (nth 7 xd))
        )
        (setq pipeEn   nil
              pipeSize ""
        )
      )
      (list name rtype area chwEn chwGpm pipeEn pipeSize)
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
(defun rt-do-export (/ ss i e edata xd row rows hasChw hasPipe csvPath fp rowIdx line)
  (princ "\nSelect room tags to export (or press Enter to export ALL): ")
  (setq ss (ssget '((0 . "MTEXT") (-3 ("ROOMTAG_DATA")))))
  (if (null ss)
    (setq ss (ssget "_X" '((0 . "MTEXT") (-3 ("ROOMTAG_DATA")))))
  )
  (if (null ss)
    (princ "\nNo HVACROOM labels found in the drawing.")
    (progn
      (setq rows    nil
            hasChw  (= *rt-chw-en* "1")
            hasPipe (and (= *rt-chw-en* "1") (= *rt-pipe-en* "1"))
            i       0
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
            (if (nth 5 row) (setq hasPipe T hasChw T))
            (setq rows (cons row rows))
          )
        )
        (setq i (1+ i))
      )
      (setq rows (vl-sort rows 'rt-compare-room-rows))
      (setq csvPath (getfiled "Export HVAC Room Data to Excel (CSV)"
                              (strcat (getvar "DWGPREFIX") "HvacRooms.csv")
                              "csv"
                              1))
      (if csvPath
        (progn
          (setq fp (open csvPath "w"))
          (cond
            (hasPipe (write-line "Room Name,Room Type,Area (m2),CHW Flow (gpm),Pipe Size" fp))
            (hasChw  (write-line "Room Name,Room Type,Area (m2),CHW Flow (gpm)" fp))
            (T       (write-line "Room Name,Room Type,Area (m2)" fp))
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
            (if hasPipe
              (setq line
                (strcat line ","
                        (if (nth 5 r)
                          (rt-csv-escape (nth 6 r))
                          ""
                        ))
              )
            )
            (write-line line fp)
            (setq rowIdx (1+ rowIdx))
          )
          (cond
            (hasPipe
             (write-line
               (strcat "TOTAL,,=SUM(C2:C" (itoa rowIdx) "),=SUM(D2:D" (itoa rowIdx) "),")
               fp
             )
            )
            (hasChw
             (write-line
               (strcat "TOTAL,,=SUM(C2:C" (itoa rowIdx) "),=SUM(D2:D" (itoa rowIdx) ")")
               fp
             )
            )
            (T
             (write-line
               (strcat "TOTAL,,=SUM(C2:C" (itoa rowIdx) ")")
               fp
             )
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
