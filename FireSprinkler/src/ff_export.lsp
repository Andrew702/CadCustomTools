;;; ==========================================================================
;;; FireSprinkler/src/ff_export.lsp - Natural-Sorted CSV Export (NFPA 13)
;;; ==========================================================================

;; Escape CSV field per RFC-4180
(defun ff-csv-escape (str)
  (if (or (vl-string-search "," str) (vl-string-search "\"" str))
    (strcat "\"" (vl-string-subst "\"\"" "\"" str) "\"")
    str
  )
)

;; Parse FFSPRINKLER_DATA XDATA into a row list:
;;   (roomName rtype areaM2 hazardLabel covArea sprCount)
(defun ff-parse-xdata (xd)
  (if (and xd (>= (length xd) 6))
    (list
      (cdr (nth 0 xd))
      (cdr (nth 1 xd))
      (cdr (nth 2 xd))
      (cdr (nth 3 xd))
      (cdr (nth 4 xd))
      (cdr (nth 5 xd))
    )
    nil
  )
)

;; Build natural sort key from room name like "Office#12":
;;   (upperPrefix hasNum numVal upperFullName)
(defun ff-room-sort-key (name / up pos prefix numStr)
  (setq up  (strcase name)
        pos (vl-string-position (ascii "#") up nil T)
  )
  (if pos
    (progn
      (setq prefix (if (> pos 0) (substr up 1 pos) "")
            numStr (substr up (+ pos 2))
      )
      (if (ff-valid-uint-p numStr)
        (list prefix T (atoi numStr) up)
        (list up nil 0 up)
      )
    )
    (list up nil 0 up)
  )
)

;; Natural comparator for exported room rows
(defun ff-compare-room-rows (a b / ka kb)
  (setq ka (ff-room-sort-key (car a))
        kb (ff-room-sort-key (car b))
  )
  (cond
    ((< (nth 0 ka) (nth 0 kb)) T)
    ((> (nth 0 ka) (nth 0 kb)) nil)
    ((and (nth 1 ka) (nth 1 kb)) (< (nth 2 ka) (nth 2 kb)))
    (T (< (nth 3 ka) (nth 3 kb)))
  )
)

;; Export tagged rooms to Excel-compatible CSV
(defun ff-do-export (/ ss i e edata xd row rows csvPath fp rowIdx line)
  (princ "\nSelect FireSprinkler tags to export (or press Enter to export ALL): ")
  (setq ss (ssget '((0 . "MTEXT") (-3 ("FFSPRINKLER_DATA")))))
  (if (null ss)
    (setq ss (ssget "_X" '((0 . "MTEXT") (-3 ("FFSPRINKLER_DATA")))))
  )
  (if (null ss)
    (princ "\nNo FFSPRINKLER labels found in the drawing.")
    (progn
      (setq rows nil
            i    0
      )
      (repeat (sslength ss)
        (setq e     (ssname ss i)
              edata (entget e '("FFSPRINKLER_DATA"))
              xd    (cdr (assoc "FFSPRINKLER_DATA" (cdr (assoc -3 edata))))
              row   (ff-parse-xdata xd)
        )
        (if row
          (setq rows (cons row rows))
        )
        (setq i (1+ i))
      )
      (setq rows (vl-sort rows 'ff-compare-room-rows))
      (setq csvPath (getfiled "Export Fire Sprinkler Schedule to Excel (CSV)"
                              (strcat (getvar "DWGPREFIX") "FireSprinklers.csv")
                              "csv"
                              1))
      (if csvPath
        (progn
          (setq fp (open csvPath "w"))
          (write-line "Room Name,Room Type,Area (m2),Hazard,Sprinklers" fp)
          (setq rowIdx 1)
          (foreach r rows
            (setq line
              (strcat (ff-csv-escape (nth 0 r)) ","
                      (ff-csv-escape (nth 1 r)) ","
                      (rtos (nth 2 r) 2 *ff-prec*) ","
                      (ff-csv-escape (nth 3 r)) ","
                      (itoa (nth 5 r)))
            )
            (write-line line fp)
            (setq rowIdx (1+ rowIdx))
          )
          (write-line
            (strcat "TOTAL,,=SUM(C2:C" (itoa rowIdx) "),,=SUM(E2:E" (itoa rowIdx) ")")
            fp
          )
          (close fp)
          (princ (strcat "\nExported " (itoa (length rows)) " rooms to: " csvPath))
        )
      )
    )
  )
)

(princ)
