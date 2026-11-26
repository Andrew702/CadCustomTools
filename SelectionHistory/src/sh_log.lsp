;;; ==========================================================================
;;; SelectionHistory/src/sh_log.lsp - Structured JSONL Debug Logger (ZWCAD)
;;; Command: SHDEBUG
;;; ==========================================================================

;; Persistent session state (preserved across APPLOAD reloads)
(if (not (boundp '*sh-log-enabled*))     (setq *sh-log-enabled* T))
(if (not (boundp '*sh-log-run-counter*)) (setq *sh-log-run-counter* 0))
(if (not (boundp '*sh-log-event-total*)) (setq *sh-log-event-total* 0))
(if (not (boundp '*sh-log-run-id*))      (setq *sh-log-run-id* nil))
(if (not (boundp '*sh-log-seq*))         (setq *sh-log-seq* 0))
(if (not (boundp '*sh-log-reactor-seq*)) (setq *sh-log-reactor-seq* 0))
(if (not (boundp '*sh-log-run-ms*))      (setq *sh-log-run-ms* 0))
(if (not (boundp '*sh-log-cmd*))         (setq *sh-log-cmd* "SH"))

;; --------------------------------------------------------------------------
;; Timestamp & Run ID Helpers (DIMZIN-safe)
;; --------------------------------------------------------------------------

(defun sh-log-now (/ raw dotPos dPart tPart)
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

(defun sh-log-bool (v)
  (if v 'true 'false)
)

;; --------------------------------------------------------------------------
;; JSON Serialization Helpers (RFC-8259 compliant, DIMZIN-safe)
;; --------------------------------------------------------------------------

(defun sh-log-json-escape (str / i len ch code out)
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

(defun sh-log-real->json (val / s)
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

(defun sh-log-json-val (val / parts item)
  (cond
    ((null val)           "null")
    ((eq val 'true)       "true")
    ((eq val 'false)      "false")
    ((eq val T)           "true")
    ((eq (type val) 'INT) (itoa val))
    ((eq (type val) 'REAL)(sh-log-real->json val))
    ((eq (type val) 'STR) (strcat "\"" (sh-log-json-escape val) "\""))
    ((listp val)
     (if (and (car val) (listp (car val)) (eq (type (caar val)) 'STR))
       (sh-log-json-obj val)
       (progn
         (setq parts "")
         (foreach item val
           (setq parts
             (if (= parts "")
               (sh-log-json-val item)
               (strcat parts "," (sh-log-json-val item))
             )
           )
         )
         (strcat "[" parts "]")
       )
     )
    )
    (T (strcat "\"" (sh-log-json-escape (vl-princ-to-string val)) "\""))
  )
)

(defun sh-log-json-obj (alist / parts pair)
  (if (null alist)
    "{}"
    (progn
      (setq parts "")
      (foreach pair alist
        (if (and pair (eq (type (car pair)) 'STR))
          (setq parts
            (if (= parts "")
              (strcat "\"" (sh-log-json-escape (car pair)) "\":" (sh-log-json-val (cdr pair)))
              (strcat parts ",\"" (sh-log-json-escape (car pair)) "\":" (sh-log-json-val (cdr pair)))
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

(defun sh-log-strip-slash (dir)
  (if (and dir (> (strlen dir) 1) (member (substr dir (strlen dir) 1) '("\\" "/")))
    (substr dir 1 (1- (strlen dir)))
    dir
  )
)

(defun sh-log-dir (/ baseDir logDir)
  (setq baseDir (if (and *sh-root-dir* (> (strlen *sh-root-dir*) 0))
                  *sh-root-dir*
                  (getvar "TEMPPREFIX")
                )
        logDir  (strcat (sh-log-strip-slash baseDir) "/logs")
  )
  (if (not (vl-file-directory-p logDir))
    (vl-catch-all-apply 'vl-mkdir (list logDir))
  )
  (if (vl-file-directory-p logDir)
    (strcat logDir "/")
    (getvar "TEMPPREFIX")
  )
)

(defun sh-log-file-path (/ nowInfo)
  (setq nowInfo (sh-log-now))
  (strcat (sh-log-dir) "selectionhistory_" (car nowInfo) ".jsonl")
)

;; --------------------------------------------------------------------------
;; Core Write & Lifecycle API (Zero-Crash Guaranteed)
;; --------------------------------------------------------------------------

(defun sh-log-write-line (line / path fp)
  (setq path (sh-log-file-path))
  (setq fp (open path "a"))
  (if fp
    (progn
      (write-line line fp)
      (close fp)
      (setq *sh-log-event-total* (1+ *sh-log-event-total*))
      T
    )
    nil
  )
)

(defun sh-log-emit (level eventName dataAlist / nowInfo ms dwg line)
  (if *sh-log-enabled*
    (vl-catch-all-apply
      '(lambda ()
         (setq nowInfo      (sh-log-now)
               ms           (getvar "MILLISECS")
               dwg          (getvar "DWGNAME")
               *sh-log-seq* (1+ *sh-log-seq*)
         )
         (if (null *sh-log-run-id*)
           (setq *sh-log-run-id* (strcat "sh-init-" (car nowInfo)))
         )
         (setq line
           (sh-log-json-obj
             (list
               (cons "ts"     (cadr nowInfo))
               (cons "ms"     ms)
               (cons "tool"   "SelectionHistory")
               (cons "run_id" *sh-log-run-id*)
               (cons "seq"    *sh-log-seq*)
               (cons "dwg"    (if dwg dwg ""))
               (cons "cmd"    (if *sh-log-cmd* *sh-log-cmd* "SH"))
               (cons "level"  level)
               (cons "event"  eventName)
               (cons "data"   (if dataAlist dataAlist '()))
             )
           )
         )
         (sh-log-write-line line)
       )
      nil
    )
  )
)

;; Background reactor events use a dedicated session stream so they never
;; interfere with explicit command run_id / seq pairs.
(defun sh-log-reactor-event (level eventName dataAlist / nowInfo ms dwg line)
  (if *sh-log-enabled*
    (vl-catch-all-apply
      '(lambda ()
         (setq nowInfo              (sh-log-now)
               ms                   (getvar "MILLISECS")
               dwg                  (getvar "DWGNAME")
               *sh-log-reactor-seq* (1+ *sh-log-reactor-seq*)
         )
         (setq line
           (sh-log-json-obj
             (list
               (cons "ts"     (cadr nowInfo))
               (cons "ms"     ms)
               (cons "tool"   "SelectionHistory")
               (cons "run_id" (strcat "sh-reactor-" (car nowInfo)))
               (cons "seq"    *sh-log-reactor-seq*)
               (cons "dwg"    (if dwg dwg ""))
               (cons "cmd"    "REACTOR")
               (cons "level"  level)
               (cons "event"  eventName)
               (cons "data"   (if dataAlist dataAlist '()))
             )
           )
         )
         (sh-log-write-line line)
       )
      nil
    )
  )
)

(defun sh-log-begin (cmdName dataAlist / nowInfo rawTime)
  (setq *sh-log-run-counter* (1+ *sh-log-run-counter*)
        nowInfo              (sh-log-now)
        rawTime              (vl-string-subst "" ":" (vl-string-subst "" ":" (substr (cadr nowInfo) 12)))
        *sh-log-run-id*      (strcat "sh-" (car nowInfo) "-" rawTime "-" (itoa *sh-log-run-counter*))
        *sh-log-seq*         0
        *sh-log-run-ms*      (getvar "MILLISECS")
        *sh-log-cmd*         (if cmdName cmdName "SH")
  )
  (sh-log-emit "INFO" "RUN_START"
    (append
      (list
        (cons "pickfirst"   (getvar "PICKFIRST"))
        (cons "history_len" (length *sh-history*))
      )
      dataAlist
    )
  )
)

(defun sh-log-event (level eventName dataAlist)
  (sh-log-emit level eventName dataAlist)
)

(defun sh-log-end (outcome dataAlist / elapsed)
  (setq elapsed (- (getvar "MILLISECS") *sh-log-run-ms*))
  (if (< elapsed 0) (setq elapsed 0))
  (sh-log-emit "INFO" "RUN_END"
    (append
      (list
        (cons "outcome"     outcome)
        (cons "elapsed_ms"  elapsed)
        (cons "history_len" (length *sh-history*))
      )
      dataAlist
    )
  )
)

(defun sh-log-error (msg / elapsed)
  (setq elapsed (- (getvar "MILLISECS") *sh-log-run-ms*))
  (if (< elapsed 0) (setq elapsed 0))
  (sh-log-emit "ERROR" "ERROR"
    (list
      (cons "message"    (if msg msg ""))
      (cons "elapsed_ms" elapsed)
    )
  )
)

;; --------------------------------------------------------------------------
;; Interactive Debug Control Command: SHDEBUG
;; --------------------------------------------------------------------------

(defun sh-log-debug-cmd (/ kw logDir files deleted f)
  (initget "On Off Path Clear Status")
  (setq kw (getkword "\nSelectionHistory Debug Log [On/Off/Path/Clear/Status] <Status>: "))
  (if (null kw) (setq kw "Status"))
  (cond
    ((= kw "On")
     (setq *sh-log-enabled* T)
     (sh-log-reactor-event "INFO" "DEBUG_TOGGLE" (list (cons "enabled" 'true)))
     (princ (strcat "\nSelectionHistory debug logging ENABLED -> " (sh-log-file-path)))
    )
    ((= kw "Off")
     (sh-log-reactor-event "INFO" "DEBUG_TOGGLE" (list (cons "enabled" 'false)))
     (setq *sh-log-enabled* nil)
     (princ "\nSelectionHistory debug logging DISABLED.")
    )
    ((= kw "Path")
     (princ (strcat "\nSelectionHistory debug log path: " (sh-log-file-path)))
    )
    ((= kw "Clear")
     (setq logDir  (sh-log-dir)
           files   (vl-directory-files logDir "selectionhistory_*.jsonl" 1)
           deleted 0
     )
     (foreach f files
       (if (vl-file-delete (strcat logDir f))
         (setq deleted (1+ deleted))
       )
     )
     (princ (strcat "\nSelectionHistory debug logs cleared (" (itoa deleted) " file(s) deleted)."))
    )
    (T
     (princ (strcat "\nSelectionHistory Debug Log: " (if *sh-log-enabled* "ON" "OFF")
                    " | Events this session: " (itoa *sh-log-event-total*)
                    " | Slots used: " (itoa (length *sh-history*)) "/" (itoa *sh-max-slots*)
                    "\nLog file: " (sh-log-file-path)))
    )
  )
  (princ)
)

(princ)
