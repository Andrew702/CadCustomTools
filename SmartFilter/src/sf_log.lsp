;;; ==========================================================================
;;; SmartFilter/src/sf_log.lsp - Structured JSONL Debug Logger for SmartFilter
;;; Command: SFDEBUG
;;; ==========================================================================

;; Persistent session state (preserved across APPLOAD reloads)
(if (not (boundp '*sf-log-enabled*))     (setq *sf-log-enabled* T))
(if (not (boundp '*sf-log-run-counter*)) (setq *sf-log-run-counter* 0))
(if (not (boundp '*sf-log-event-total*)) (setq *sf-log-event-total* 0))
(if (not (boundp '*sf-log-run-id*))      (setq *sf-log-run-id* nil))
(if (not (boundp '*sf-log-seq*))         (setq *sf-log-seq* 0))
(if (not (boundp '*sf-log-run-ms*))      (setq *sf-log-run-ms* 0))
(if (not (boundp '*sf-log-cmd*))         (setq *sf-log-cmd* "SMARTFILTER"))

;; --------------------------------------------------------------------------
;; Timestamp & Run ID Helpers (DIMZIN-safe)
;; --------------------------------------------------------------------------

(defun sf-log-now (/ raw dotPos dPart tPart)
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

(defun sf-log-bool (v)
  (if v 'true 'false)
)

;; --------------------------------------------------------------------------
;; JSON Serialization Helpers (RFC-8259 compliant, DIMZIN-safe)
;; --------------------------------------------------------------------------

(defun sf-log-json-escape (str / i len ch code out)
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

(defun sf-log-real->json (val / s)
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

(defun sf-log-json-val (val / parts item)
  (cond
    ((null val)           "null")
    ((eq val 'true)       "true")
    ((eq val 'false)      "false")
    ((eq val T)           "true")
    ((eq (type val) 'INT) (itoa val))
    ((eq (type val) 'REAL)(sf-log-real->json val))
    ((eq (type val) 'STR) (strcat "\"" (sf-log-json-escape val) "\""))
    ((listp val)
     (if (and (car val) (listp (car val)) (eq (type (caar val)) 'STR))
       (sf-log-json-obj val)
       (progn
         (setq parts "")
         (foreach item val
           (setq parts
             (if (= parts "")
               (sf-log-json-val item)
               (strcat parts "," (sf-log-json-val item))
             )
           )
         )
         (strcat "[" parts "]")
       )
     )
    )
    (T (strcat "\"" (sf-log-json-escape (vl-princ-to-string val)) "\""))
  )
)

(defun sf-log-json-obj (alist / parts pair)
  (if (null alist)
    "{}"
    (progn
      (setq parts "")
      (foreach pair alist
        (if (and pair (eq (type (car pair)) 'STR))
          (setq parts
            (if (= parts "")
              (strcat "\"" (sf-log-json-escape (car pair)) "\":" (sf-log-json-val (cdr pair)))
              (strcat parts ",\"" (sf-log-json-escape (car pair)) "\":" (sf-log-json-val (cdr pair)))
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

(defun sf-log-strip-slash (dir)
  (if (and dir (> (strlen dir) 1) (member (substr dir (strlen dir) 1) '("\\" "/")))
    (substr dir 1 (1- (strlen dir)))
    dir
  )
)

(defun sf-log-dir (/ baseDir logDir)
  (setq baseDir (if (and *sf-root-dir* (> (strlen *sf-root-dir*) 0))
                  *sf-root-dir*
                  (getvar "TEMPPREFIX")
                )
        logDir  (strcat (sf-log-strip-slash baseDir) "/logs")
  )
  (if (not (vl-file-directory-p logDir))
    (vl-catch-all-apply 'vl-mkdir (list logDir))
  )
  (if (vl-file-directory-p logDir)
    (strcat logDir "/")
    (getvar "TEMPPREFIX")
  )
)

(defun sf-log-file-path (/ nowInfo)
  (setq nowInfo (sf-log-now))
  (strcat (sf-log-dir) "smartfilter_" (car nowInfo) ".jsonl")
)

;; --------------------------------------------------------------------------
;; Core Write & Lifecycle API (Zero-Crash Guaranteed)
;; --------------------------------------------------------------------------

(defun sf-log-write-line (line / path fp)
  (setq path (sf-log-file-path))
  (setq fp (open path "a"))
  (if fp
    (progn
      (write-line line fp)
      (close fp)
      (setq *sf-log-event-total* (1+ *sf-log-event-total*))
      T
    )
    nil
  )
)

(defun sf-log-emit (level eventName dataAlist / nowInfo ms dwg line)
  (if *sf-log-enabled*
    (vl-catch-all-apply
      '(lambda ()
         (setq nowInfo      (sf-log-now)
               ms           (getvar "MILLISECS")
               dwg          (getvar "DWGNAME")
               *sf-log-seq* (1+ *sf-log-seq*)
         )
         (if (null *sf-log-run-id*)
           (setq *sf-log-run-id* (strcat "sf-init-" (car nowInfo)))
         )
         (setq line
           (sf-log-json-obj
             (list
               (cons "ts"     (cadr nowInfo))
               (cons "ms"     ms)
               (cons "tool"   "SmartFilter")
               (cons "run_id" *sf-log-run-id*)
               (cons "seq"    *sf-log-seq*)
               (cons "dwg"    (if dwg dwg ""))
               (cons "cmd"    (if *sf-log-cmd* *sf-log-cmd* "SMARTFILTER"))
               (cons "level"  level)
               (cons "event"  eventName)
               (cons "data"   (if dataAlist dataAlist '()))
             )
           )
         )
         (sf-log-write-line line)
       )
      nil
    )
  )
)

(defun sf-log-begin (cmdName dataAlist / nowInfo rawTime)
  (setq *sf-log-run-counter* (1+ *sf-log-run-counter*)
        nowInfo              (sf-log-now)
        rawTime              (vl-string-subst "" ":" (vl-string-subst "" ":" (substr (cadr nowInfo) 12)))
        *sf-log-run-id*      (strcat "sf-" (car nowInfo) "-" rawTime "-" (itoa *sf-log-run-counter*))
        *sf-log-seq*         0
        *sf-log-run-ms*      (getvar "MILLISECS")
        *sf-log-cmd*         (if cmdName cmdName "SMARTFILTER")
  )
  (sf-log-emit "INFO" "RUN_START"
    (append
      (list
        (cons "pickfirst" (getvar "PICKFIRST"))
        (cons "by_layer"  (sf-log-bool (= *sf-by-layer* "1")))
      )
      dataAlist
    )
  )
)

(defun sf-log-event (level eventName dataAlist)
  (sf-log-emit level eventName dataAlist)
)

(defun sf-log-end (outcome dataAlist / elapsed)
  (setq elapsed (- (getvar "MILLISECS") *sf-log-run-ms*))
  (if (< elapsed 0) (setq elapsed 0))
  (sf-log-emit "INFO" "RUN_END"
    (append
      (list
        (cons "outcome"    outcome)
        (cons "elapsed_ms" elapsed)
      )
      dataAlist
    )
  )
)

(defun sf-log-error (msg / elapsed)
  (setq elapsed (- (getvar "MILLISECS") *sf-log-run-ms*))
  (if (< elapsed 0) (setq elapsed 0))
  (sf-log-emit "ERROR" "ERROR"
    (list
      (cons "message"    (if msg msg ""))
      (cons "elapsed_ms" elapsed)
    )
  )
)

;; --------------------------------------------------------------------------
;; Interactive Debug Control Command: SFDEBUG
;; --------------------------------------------------------------------------

(defun sf-log-debug-cmd (/ kw logDir files deleted f)
  (initget "On Off Path Clear Status")
  (setq kw (getkword "\nSmartFilter Debug Log [On/Off/Path/Clear/Status] <Status>: "))
  (if (null kw) (setq kw "Status"))
  (cond
    ((= kw "On")
     (setq *sf-log-enabled* T)
     (sf-log-emit "INFO" "DEBUG_TOGGLE" (list (cons "enabled" 'true)))
     (princ (strcat "\nSmartFilter debug logging ENABLED -> " (sf-log-file-path)))
    )
    ((= kw "Off")
     (sf-log-emit "INFO" "DEBUG_TOGGLE" (list (cons "enabled" 'false)))
     (setq *sf-log-enabled* nil)
     (princ "\nSmartFilter debug logging DISABLED.")
    )
    ((= kw "Path")
     (princ (strcat "\nSmartFilter debug log path: " (sf-log-file-path)))
    )
    ((= kw "Clear")
     (setq logDir  (sf-log-dir)
           files   (vl-directory-files logDir "smartfilter_*.jsonl" 1)
           deleted 0
     )
     (foreach f files
       (if (vl-file-delete (strcat logDir f))
         (setq deleted (1+ deleted))
       )
     )
     (princ (strcat "\nSmartFilter debug logs cleared (" (itoa deleted) " file(s) deleted)."))
    )
    (T
     (princ (strcat "\nSmartFilter Debug Log: " (if *sf-log-enabled* "ON" "OFF")
                    " | Events this session: " (itoa *sf-log-event-total*)
                    " | Last run: " (if *sf-log-run-id* *sf-log-run-id* "(none)")
                    "\nLog file: " (sf-log-file-path)))
    )
  )
  (princ)
)

(princ)
