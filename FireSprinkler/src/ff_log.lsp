;;; ==========================================================================
;;; FireSprinkler/src/ff_log.lsp - Structured JSONL Debug Logger (NFPA 13)
;;; Command: FFDEBUG
;;; ==========================================================================

;; Persistent session state (preserved across APPLOAD reloads)
(if (not (boundp '*ff-log-enabled*))     (setq *ff-log-enabled* T))
(if (not (boundp '*ff-log-run-counter*)) (setq *ff-log-run-counter* 0))
(if (not (boundp '*ff-log-event-total*)) (setq *ff-log-event-total* 0))
(if (not (boundp '*ff-log-run-id*))      (setq *ff-log-run-id* nil))
(if (not (boundp '*ff-log-seq*))         (setq *ff-log-seq* 0))
(if (not (boundp '*ff-log-run-ms*))      (setq *ff-log-run-ms* 0))
(if (not (boundp '*ff-log-cmd*))         (setq *ff-log-cmd* "FFSPRINKLER"))

;; --------------------------------------------------------------------------
;; Timestamp & Run ID Helpers (DIMZIN-safe)
;; --------------------------------------------------------------------------

(defun ff-log-now (/ raw dotPos dPart tPart)
  (setq raw    (rtos (getvar "CDATE") 2 6)
        dotPos (vl-string-position (ascii ".") raw)
  )
  (if dotPos
    (setq dPart (substr raw 1 dotPos)
          tPart (substr raw (+ dotPos 2))
    )
    (setq dPart raw
          tPart "000000"
    )
  )
  (while (< (strlen dPart) 8) (setq dPart (strcat dPart "0")))
  (while (< (strlen tPart) 6) (setq tPart (strcat tPart "0")))
  (list
    (substr dPart 1 8)
    (strcat (substr dPart 1 4) "-" (substr dPart 5 2) "-" (substr dPart 7 2)
            "T"
            (substr tPart 1 2) ":" (substr tPart 3 2) ":" (substr tPart 5 2))
  )
)

(defun ff-log-bool (v)
  (if v 'true 'false)
)

(defun ff-log-handle (ent / ed)
  (if (and ent (eq (type ent) 'ENAME) (setq ed (entget ent)))
    (cdr (assoc 5 ed))
    nil
  )
)

;; --------------------------------------------------------------------------
;; JSON Serialization Helpers (RFC-8259 compliant, DIMZIN-safe)
;; --------------------------------------------------------------------------

(defun ff-log-json-escape (str / i len ch code out)
  (if (not (eq (type str) 'STR))
    ""
    (progn
      (setq i   1
            len (strlen str)
            out ""
      )
      (while (<= i len)
        (setq ch   (substr str i 1)
              code (ascii ch)
        )
        (cond
          ((= ch "\\") (setq out (strcat out "\\\\")))
          ((= ch "\"") (setq out (strcat out "\\\"")))
          ((= code 10) (setq out (strcat out "\\n")))
          ((= code 13) (setq out (strcat out "\\r")))
          ((= code 9)  (setq out (strcat out "\\t")))
          ((< code 32) (setq out (strcat out " ")))
          (T           (setq out (strcat out ch)))
        )
        (setq i (1+ i))
      )
      out
    )
  )
)

(defun ff-log-real->json (val / s)
  (setq s (rtos (float val) 2 8))
  (cond
    ((= (substr s 1 1) ".")  (setq s (strcat "0" s)))
    ((= (substr s 1 2) "-.") (setq s (strcat "-0." (substr s 3))))
  )
  (if (= (substr s (strlen s) 1) ".")
    (setq s (strcat s "0"))
  )
  s
)

(defun ff-log-json-val (val / parts item)
  (cond
    ((null val)           "null")
    ((eq val 'true)       "true")
    ((eq val 'false)      "false")
    ((eq val T)           "true")
    ((eq (type val) 'INT) (itoa val))
    ((eq (type val) 'REAL)(ff-log-real->json val))
    ((eq (type val) 'STR) (strcat "\"" (ff-log-json-escape val) "\""))
    ((listp val)
     (if (and (car val) (listp (car val)) (eq (type (caar val)) 'STR))
       (ff-log-json-obj val)
       (progn
         (setq parts "")
         (foreach item val
           (setq parts
             (if (= parts "")
               (ff-log-json-val item)
               (strcat parts "," (ff-log-json-val item))
             )
           )
         )
         (strcat "[" parts "]")
       )
     )
    )
    (T (strcat "\"" (ff-log-json-escape (vl-princ-to-string val)) "\""))
  )
)

(defun ff-log-json-obj (alist / parts pair)
  (if (null alist)
    "{}"
    (progn
      (setq parts "")
      (foreach pair alist
        (if (and pair (eq (type (car pair)) 'STR))
          (setq parts
            (if (= parts "")
              (strcat "\"" (ff-log-json-escape (car pair)) "\":" (ff-log-json-val (cdr pair)))
              (strcat parts ",\"" (ff-log-json-escape (car pair)) "\":" (ff-log-json-val (cdr pair)))
            )
          )
        )
      )
      (strcat "{" parts "}")
    )
  )
)

;; --------------------------------------------------------------------------
;; Log Directory & File Path Resolution (with TEMPPREFIX fallback)
;; --------------------------------------------------------------------------

(defun ff-log-strip-slash (dir)
  (if (and dir (> (strlen dir) 1) (member (substr dir (strlen dir) 1) '("\\" "/")))
    (substr dir 1 (1- (strlen dir)))
    dir
  )
)

(defun ff-log-dir (/ baseDir logDir)
  (setq baseDir (if (and *ff-root-dir* (> (strlen *ff-root-dir*) 0))
                  *ff-root-dir*
                  (getvar "TEMPPREFIX")
                )
        logDir  (strcat (ff-log-strip-slash baseDir) "/logs")
  )
  (if (not (vl-file-directory-p logDir))
    (vl-catch-all-apply 'vl-mkdir (list logDir))
  )
  (if (vl-file-directory-p logDir)
    (strcat logDir "/")
    (getvar "TEMPPREFIX")
  )
)

(defun ff-log-file-path (/ nowInfo)
  (setq nowInfo (ff-log-now))
  (strcat (ff-log-dir) "firesprinkler_" (car nowInfo) ".jsonl")
)

;; --------------------------------------------------------------------------
;; Core Write & Lifecycle API (Zero-Crash Guaranteed)
;; --------------------------------------------------------------------------

(defun ff-log-write-line (line / path fp)
  (setq path (ff-log-file-path))
  (setq fp (open path "a"))
  (if fp
    (progn
      (write-line line fp)
      (close fp)
      (setq *ff-log-event-total* (1+ *ff-log-event-total*))
      T
    )
    nil
  )
)

(defun ff-log-emit (level eventName dataAlist / nowInfo ms dwg line)
  (if *ff-log-enabled*
    (vl-catch-all-apply
      '(lambda ()
         (setq nowInfo      (ff-log-now)
               ms           (getvar "MILLISECS")
               dwg          (getvar "DWGNAME")
               *ff-log-seq* (1+ *ff-log-seq*)
         )
         (if (null *ff-log-run-id*)
           (setq *ff-log-run-id* (strcat "ff-init-" (car nowInfo)))
         )
         (setq line
           (ff-log-json-obj
             (list
               (cons "ts"     (cadr nowInfo))
               (cons "ms"     ms)
               (cons "tool"   "FireSprinkler")
               (cons "run_id" *ff-log-run-id*)
               (cons "seq"    *ff-log-seq*)
               (cons "dwg"    (if dwg dwg ""))
               (cons "cmd"    (if *ff-log-cmd* *ff-log-cmd* "FFSPRINKLER"))
               (cons "level"  level)
               (cons "event"  eventName)
               (cons "data"   (if dataAlist dataAlist '()))
             )
           )
         )
         (ff-log-write-line line)
       )
      nil
    )
  )
)

(defun ff-log-begin (cmdName dataAlist / nowInfo rawTime)
  (setq *ff-log-run-counter* (1+ *ff-log-run-counter*)
        nowInfo              (ff-log-now)
        rawTime              (vl-string-subst "" ":" (vl-string-subst "" ":" (substr (cadr nowInfo) 12)))
        *ff-log-run-id*      (strcat "ff-" (car nowInfo) "-" rawTime "-" (itoa *ff-log-run-counter*))
        *ff-log-seq*         0
        *ff-log-run-ms*      (getvar "MILLISECS")
        *ff-log-cmd*         (if cmdName cmdName "FFSPRINKLER")
  )
  (ff-log-emit "INFO" "RUN_START"
    (append
      (list
        (cons "clayer"   (getvar "CLAYER"))
        (cons "textsize" (getvar "TEXTSIZE"))
        (cons "insunits" (getvar "INSUNITS"))
      )
      dataAlist
    )
  )
)

(defun ff-log-event (level eventName dataAlist)
  (ff-log-emit level eventName dataAlist)
)

(defun ff-log-end (outcome dataAlist / elapsed)
  (setq elapsed (- (getvar "MILLISECS") *ff-log-run-ms*))
  (if (< elapsed 0) (setq elapsed 0))
  (ff-log-emit "INFO" "RUN_END"
    (append
      (list
        (cons "outcome"    outcome)
        (cons "elapsed_ms" elapsed)
      )
      dataAlist
    )
  )
)

(defun ff-log-error (msg / elapsed)
  (setq elapsed (- (getvar "MILLISECS") *ff-log-run-ms*))
  (if (< elapsed 0) (setq elapsed 0))
  (ff-log-emit "ERROR" "ERROR"
    (list
      (cons "message"    (if msg msg ""))
      (cons "elapsed_ms" elapsed)
    )
  )
)

;; --------------------------------------------------------------------------
;; Interactive Debug Control Command: FFDEBUG
;; --------------------------------------------------------------------------

(defun ff-log-debug-cmd (/ kw logDir files deleted f)
  (initget "On Off Path Clear Status")
  (setq kw (getkword "\nFireSprinkler Debug Log [On/Off/Path/Clear/Status] <Status>: "))
  (if (null kw) (setq kw "Status"))
  (cond
    ((= kw "On")
     (setq *ff-log-enabled* T)
     (ff-log-emit "INFO" "DEBUG_TOGGLE" (list (cons "enabled" 'true)))
     (princ (strcat "\nFireSprinkler debug logging ENABLED -> " (ff-log-file-path)))
    )
    ((= kw "Off")
     (ff-log-emit "INFO" "DEBUG_TOGGLE" (list (cons "enabled" 'false)))
     (setq *ff-log-enabled* nil)
     (princ "\nFireSprinkler debug logging DISABLED.")
    )
    ((= kw "Path")
     (princ (strcat "\nFireSprinkler debug log path: " (ff-log-file-path)))
    )
    ((= kw "Clear")
     (setq logDir  (ff-log-dir)
           files   (vl-directory-files logDir "firesprinkler_*.jsonl" 1)
           deleted 0
     )
     (foreach f files
       (if (vl-file-delete (strcat logDir f))
         (setq deleted (1+ deleted))
       )
     )
     (princ (strcat "\nFireSprinkler debug logs cleared (" (itoa deleted) " file(s) deleted)."))
    )
    (T
     (princ (strcat "\nFireSprinkler Debug Log: " (if *ff-log-enabled* "ON" "OFF")
                    " | Events this session: " (itoa *ff-log-event-total*)
                    " | Last run: " (if *ff-log-run-id* *ff-log-run-id* "(none)")
                    "\nLog file: " (ff-log-file-path)))
    )
  )
  (princ)
)

(princ)
