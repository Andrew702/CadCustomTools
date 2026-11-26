;;; ==========================================================================
;;; HvacRoom/src/rt_log.lsp - Structured JSONL Debug Logger for HvacRoom
;;; Command: RTDEBUG
;;; ==========================================================================

;; Persistent session state (preserved across APPLOAD reloads)
(if (not (boundp '*rt-log-enabled*))     (setq *rt-log-enabled* T))
(if (not (boundp '*rt-log-run-counter*)) (setq *rt-log-run-counter* 0))
(if (not (boundp '*rt-log-event-total*)) (setq *rt-log-event-total* 0))
(if (not (boundp '*rt-log-run-id*))      (setq *rt-log-run-id* nil))
(if (not (boundp '*rt-log-seq*))         (setq *rt-log-seq* 0))
(if (not (boundp '*rt-log-run-ms*))      (setq *rt-log-run-ms* 0))
(if (not (boundp '*rt-log-cmd*))         (setq *rt-log-cmd* "HVACROOM"))

;; --------------------------------------------------------------------------
;; Timestamp & Run ID Helpers (DIMZIN-safe)
;; --------------------------------------------------------------------------

;; Return (list dateStr isoTs) from CDATE, e.g. ("20261126" "2026-11-26T00:15:30")
(defun rt-log-now (/ raw dotPos dPart tPart)
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

;; Return explicit boolean marker for JSON serializer ('true or 'false)
(defun rt-log-bool (v)
  (if v 'true 'false)
)

;; Safely extract DXF handle string from an ename, or nil
(defun rt-log-handle (ent / ed)
  (if (and ent (eq (type ent) 'ENAME) (setq ed (entget ent)))
    (cdr (assoc 5 ed))
    nil
  )
)

;; --------------------------------------------------------------------------
;; JSON Serialization Helpers (RFC-8259 compliant, DIMZIN-safe)
;; --------------------------------------------------------------------------

(defun rt-log-json-escape (str / i len ch code out)
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

;; Format a REAL as a valid JSON number (guards against DIMZIN ".5" / "-.5" / "1.")
(defun rt-log-real->json (val / s)
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

;; Detect whether a non-empty list is a string-keyed alist ((k . v) ...)
(defun rt-log-alist-p (lst)
  (and (listp lst)
       (car lst)
       (listp (car lst))
       (eq (type (caar lst)) 'STR)
       (not (listp (cdar lst)))
  )
)

;; Serialize any supported AutoLISP value to JSON
(defun rt-log-json-val (val / parts item)
  (cond
    ((null val)           "null")
    ((eq val 'true)       "true")
    ((eq val 'false)      "false")
    ((eq val T)           "true")
    ((eq (type val) 'INT) (itoa val))
    ((eq (type val) 'REAL)(rt-log-real->json val))
    ((eq (type val) 'STR) (strcat "\"" (rt-log-json-escape val) "\""))
    ((listp val)
     (if (and (car val) (listp (car val)) (eq (type (caar val)) 'STR))
       (rt-log-json-obj val)
       (progn
         (setq parts "")
         (foreach item val
           (setq parts
             (if (= parts "")
               (rt-log-json-val item)
               (strcat parts "," (rt-log-json-val item))
             )
           )
         )
         (strcat "[" parts "]")
       )
     )
    )
    (T (strcat "\"" (rt-log-json-escape (vl-princ-to-string val)) "\""))
  )
)

;; Serialize an alist (("key" . val) ...) to a JSON object string
(defun rt-log-json-obj (alist / parts pair)
  (if (null alist)
    "{}"
    (progn
      (setq parts "")
      (foreach pair alist
        (if (and pair (eq (type (car pair)) 'STR))
          (setq parts
            (if (= parts "")
              (strcat "\"" (rt-log-json-escape (car pair)) "\":" (rt-log-json-val (cdr pair)))
              (strcat parts ",\"" (rt-log-json-escape (car pair)) "\":" (rt-log-json-val (cdr pair)))
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

(defun rt-log-strip-slash (dir)
  (if (and dir (> (strlen dir) 1) (member (substr dir (strlen dir) 1) '("\\" "/")))
    (substr dir 1 (1- (strlen dir)))
    dir
  )
)

(defun rt-log-dir (/ baseDir logDir)
  (setq baseDir (if (and *rt-root-dir* (> (strlen *rt-root-dir*) 0))
                  *rt-root-dir*
                  (getvar "TEMPPREFIX")
                )
        logDir  (strcat (rt-log-strip-slash baseDir) "/logs")
  )
  (if (not (vl-file-directory-p logDir))
    (vl-catch-all-apply 'vl-mkdir (list logDir))
  )
  (if (vl-file-directory-p logDir)
    (strcat logDir "/")
    (getvar "TEMPPREFIX")
  )
)

(defun rt-log-file-path (/ nowInfo)
  (setq nowInfo (rt-log-now))
  (strcat (rt-log-dir) "hvacroom_" (car nowInfo) ".jsonl")
)

;; --------------------------------------------------------------------------
;; Core Write & Lifecycle API (Zero-Crash Guaranteed)
;; --------------------------------------------------------------------------

(defun rt-log-write-line (line / path fp)
  (setq path (rt-log-file-path))
  (setq fp (open path "a"))
  (if fp
    (progn
      (write-line line fp)
      (close fp)
      (setq *rt-log-event-total* (1+ *rt-log-event-total*))
      T
    )
    nil
  )
)

(defun rt-log-emit (level eventName dataAlist / nowInfo ms dwg line)
  (if *rt-log-enabled*
    (vl-catch-all-apply
      '(lambda ()
         (setq nowInfo      (rt-log-now)
               ms           (getvar "MILLISECS")
               dwg          (getvar "DWGNAME")
               *rt-log-seq* (1+ *rt-log-seq*)
         )
         (if (null *rt-log-run-id*)
           (setq *rt-log-run-id* (strcat "rt-init-" (car nowInfo)))
         )
         (setq line
           (rt-log-json-obj
             (list
               (cons "ts"     (cadr nowInfo))
               (cons "ms"     ms)
               (cons "tool"   "HvacRoom")
               (cons "run_id" *rt-log-run-id*)
               (cons "seq"    *rt-log-seq*)
               (cons "dwg"    (if dwg dwg ""))
               (cons "cmd"    (if *rt-log-cmd* *rt-log-cmd* "HVACROOM"))
               (cons "level"  level)
               (cons "event"  eventName)
               (cons "data"   (if dataAlist dataAlist '()))
             )
           )
         )
         (rt-log-write-line line)
       )
      nil
    )
  )
)

(defun rt-log-begin (cmdName dataAlist / nowInfo rawTime)
  (setq *rt-log-run-counter* (1+ *rt-log-run-counter*)
        nowInfo              (rt-log-now)
        rawTime              (vl-string-subst "" ":" (vl-string-subst "" ":" (substr (cadr nowInfo) 12)))
        *rt-log-run-id*      (strcat "rt-" (car nowInfo) "-" rawTime "-" (itoa *rt-log-run-counter*))
        *rt-log-seq*         0
        *rt-log-run-ms*      (getvar "MILLISECS")
        *rt-log-cmd*         (if cmdName cmdName "HVACROOM")
  )
  (rt-log-emit "INFO" "RUN_START"
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

(defun rt-log-event (level eventName dataAlist)
  (rt-log-emit level eventName dataAlist)
)

(defun rt-log-end (outcome dataAlist / elapsed)
  (setq elapsed (- (getvar "MILLISECS") *rt-log-run-ms*))
  (if (< elapsed 0) (setq elapsed 0))
  (rt-log-emit "INFO" "RUN_END"
    (append
      (list
        (cons "outcome"    outcome)
        (cons "elapsed_ms" elapsed)
      )
      dataAlist
    )
  )
)

(defun rt-log-error (msg / elapsed)
  (setq elapsed (- (getvar "MILLISECS") *rt-log-run-ms*))
  (if (< elapsed 0) (setq elapsed 0))
  (rt-log-emit "ERROR" "ERROR"
    (list
      (cons "message"    (if msg msg ""))
      (cons "elapsed_ms" elapsed)
    )
  )
)

;; --------------------------------------------------------------------------
;; Interactive Debug Control Command: RTDEBUG
;; --------------------------------------------------------------------------

(defun rt-log-debug-cmd (/ kw logDir files deleted f)
  (initget "On Off Path Clear Status")
  (setq kw (getkword "\nHvacRoom Debug Log [On/Off/Path/Clear/Status] <Status>: "))
  (if (null kw) (setq kw "Status"))
  (cond
    ((= kw "On")
     (setq *rt-log-enabled* T)
     (rt-log-emit "INFO" "DEBUG_TOGGLE" (list (cons "enabled" 'true)))
     (princ (strcat "\nHvacRoom debug logging ENABLED -> " (rt-log-file-path)))
    )
    ((= kw "Off")
     (rt-log-emit "INFO" "DEBUG_TOGGLE" (list (cons "enabled" 'false)))
     (setq *rt-log-enabled* nil)
     (princ "\nHvacRoom debug logging DISABLED.")
    )
    ((= kw "Path")
     (princ (strcat "\nHvacRoom debug log path: " (rt-log-file-path)))
    )
    ((= kw "Clear")
     (setq logDir  (rt-log-dir)
           files   (vl-directory-files logDir "hvacroom_*.jsonl" 1)
           deleted 0
     )
     (foreach f files
       (if (vl-file-delete (strcat logDir f))
         (setq deleted (1+ deleted))
       )
     )
     (princ (strcat "\nHvacRoom debug logs cleared (" (itoa deleted) " file(s) deleted)."))
    )
    (T
     (princ (strcat "\nHvacRoom Debug Log: " (if *rt-log-enabled* "ON" "OFF")
                    " | Events this session: " (itoa *rt-log-event-total*)
                    " | Last run: " (if *rt-log-run-id* *rt-log-run-id* "(none)")
                    "\nLog file: " (rt-log-file-path)))
    )
  )
  (princ)
)

(princ)
