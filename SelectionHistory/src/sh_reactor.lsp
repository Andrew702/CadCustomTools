;;; ==========================================================================
;;; src/sh_reactor.lsp - Visual LISP Reactors for Automatic Selection Tracking
;;; ==========================================================================

(setq *sh-pf-reactor-tag*  "SH_PICKFIRST_REACTOR"
      *sh-cmd-reactor-tag* "SH_COMMAND_REACTOR"
)

;; --------------------------------------------------------------------------
;; Command Name Helpers
;; --------------------------------------------------------------------------

;; Normalize a command name string (uppercase, strip leading _, ., or C:).
(defun sh-normalize-cmd (cmd / s)
  (if (or (null cmd) (not (= (type cmd) 'STR)))
    ""
    (progn
      (setq s (strcase (vl-string-trim " \t\r\n" cmd)))
      (while (and (> (strlen s) 0) (member (substr s 1 1) '("_" "." "-")))
        (setq s (substr s 2))
      )
      (if (and (>= (strlen s) 2) (= (substr s 1 2) "C:"))
        (setq s (substr s 3))
      )
      s
    )
  )
)

;; Return T if cmd is one of SelectionHistory's own recall commands.
(defun sh-own-command-p (cmd / norm)
  (setq norm (sh-normalize-cmd cmd))
  (not (null (member norm '("SH" "SELHIST" "SH1" "SH2" "SH3" "SH4" "SH5"))))
)

;; Replace the 'source field in a snapshot alist.
(defun sh-snap-with-source (snap newSource / oldPair)
  (setq oldPair (assoc 'source snap))
  (if oldPair
    (subst (cons 'source newSource) oldPair snap)
    (cons (cons 'source newSource) snap)
  )
)

;; --------------------------------------------------------------------------
;; Reactor Event Handlers (Safe / Read-Only CAD Queries)
;; --------------------------------------------------------------------------

;; Handle :vlr-pickfirstModified (outside-command implied selection changes).
(defun sh-handle-pickfirst (/ ss snap key)
  (setq ss (ssget "_I"))
  (if (and ss (> (sslength ss) 0))
    (progn
      (setq snap (sh-ss->snapshot ss "Pickfirst"))
      (if snap
        (progn
          (setq key (cdr (assoc 'key snap)))
          (if (and *sh-suppress-key* (equal key *sh-suppress-key*))
            ;; Active grips are from a recall; keep history untouched.
            nil
            (progn
              (setq *sh-suppress-key* nil
                    *sh-pending-pf*   snap
              )
            )
          )
        )
      )
    )
    ;; Pickfirst became empty (Esc, deselect, or command start)
    (progn
      (if *sh-pending-pf*
        (progn
          (sh-push-snapshot *sh-pending-pf*)
          (setq *sh-pending-pf* nil)
        )
      )
      ;; Only clear recall suppression if no command is currently consuming it
      (if (and *sh-suppress-key* (= (getvar "CMDACTIVE") 0))
        (setq *sh-suppress-key* nil)
      )
    )
  )
  nil
)

;; Handle :vlr-commandWillStart (commit pending pickfirst before command runs).
(defun sh-handle-command-start (args / rawCmd cmdName src snap)
  (setq rawCmd  (if (and args (= (type (car args)) 'STR)) (car args) "")
        cmdName (sh-normalize-cmd rawCmd)
  )
  (if *sh-pending-pf*
    (progn
      (setq src  (if (or (= cmdName "") (sh-own-command-p cmdName))
                   "Pickfirst"
                   cmdName
                 )
            snap (sh-snap-with-source *sh-pending-pf* src)
      )
      (sh-push-snapshot snap)
      ;; Remember key so command-end _P does not duplicate this noun-verb set
      (setq *sh-last-prev-key* (cdr (assoc 'key snap))
            *sh-pending-pf*    nil
      )
    )
  )
  nil
)

;; Handle :vlr-commandEnded / :vlr-commandCancelled / :vlr-commandFailed.
(defun sh-handle-command-end (args / rawCmd cmdName ss snap key curPf)
  (setq rawCmd  (if (and args (= (type (car args)) 'STR)) (car args) "")
        cmdName (sh-normalize-cmd rawCmd)
  )
  (if (not (sh-own-command-p cmdName))
    (progn
      (setq ss (ssget "_P"))
      (if (and ss (> (sslength ss) 0))
        (progn
          (setq snap (sh-ss->snapshot ss (if (> (strlen cmdName) 0) cmdName "Command")))
          (if snap
            (progn
              (setq key (cdr (assoc 'key snap)))
              (cond
                ;; Command used a recalled slot directly: keep history order unchanged
                ((and *sh-suppress-key* (equal key *sh-suppress-key*))
                 (setq *sh-last-prev-key* key
                       *sh-suppress-key*  nil
                 )
                )
                ;; Stale _P from an earlier command (e.g. ZOOM, PAN, LINE)
                ((equal key *sh-last-prev-key*)
                 nil
                )
                ;; Genuine new selection made inside the command
                (T
                 (sh-push-snapshot snap)
                 (setq *sh-last-prev-key* key
                       *sh-suppress-key*  nil
                 )
                )
              )
            )
          )
        )
      )
      ;; Clear recall suppression if pickfirst grips are no longer active
      (if *sh-suppress-key*
        (progn
          (setq curPf (ssget "_I"))
          (if (or (null curPf) (= (sslength curPf) 0))
            (setq *sh-suppress-key* nil)
          )
        )
      )
    )
  )
  nil
)

;; --------------------------------------------------------------------------
;; Protected Reactor Callback Entry Points
;; --------------------------------------------------------------------------

(defun sh-on-pickfirst-modified (reactor args)
  (vl-catch-all-apply 'sh-handle-pickfirst nil)
  nil
)

(defun sh-on-command-start (reactor args)
  (vl-catch-all-apply 'sh-handle-command-start (list args))
  nil
)

(defun sh-on-command-end (reactor args)
  (vl-catch-all-apply 'sh-handle-command-end (list args))
  nil
)

;; --------------------------------------------------------------------------
;; Reactor Installation & Reload-Safe Cleanup
;; --------------------------------------------------------------------------

;; Remove any existing reactors of reactorType matching our tag string.
(defun sh-remove-tagged-reactors (reactorType tag / groups grp r)
  (setq groups (vlr-reactors reactorType))
  (foreach grp groups
    (foreach r (cdr grp)
      (if (and r (equal (vlr-data r) tag))
        (vlr-remove r)
      )
    )
  )
)

;; Remove both SelectionHistory reactors if present.
(defun sh-remove-reactors ()
  (sh-remove-tagged-reactors :vlr-miscellaneous-reactor *sh-pf-reactor-tag*)
  (sh-remove-tagged-reactors :vlr-command-reactor *sh-cmd-reactor-tag*)
  (setq *sh-pf-reactor*  nil
        *sh-cmd-reactor* nil
  )
  T
)

;; Install (or cleanly reinstall) both SelectionHistory reactors.
(defun sh-install-reactors (/ ss snap)
  (sh-remove-reactors)

  ;; Seed *sh-last-prev-key* on first load so pre-existing _P is not captured
  ;; by the first unrelated command (e.g. ZOOM/PAN).
  (if (null *sh-last-prev-key*)
    (progn
      (setq ss (ssget "_P"))
      (if (and ss (> (sslength ss) 0))
        (progn
          (setq snap (sh-ss->snapshot ss "Previous"))
          (if snap
            (setq *sh-last-prev-key* (cdr (assoc 'key snap)))
          )
        )
      )
    )
  )

  (setq *sh-pf-reactor*
    (vlr-miscellaneous-reactor
      *sh-pf-reactor-tag*
      '(
        (:vlr-pickfirstModified . sh-on-pickfirst-modified)
      )
    )
  )

  (setq *sh-cmd-reactor*
    (vlr-command-reactor
      *sh-cmd-reactor-tag*
      '(
        (:vlr-commandWillStart . sh-on-command-start)
        (:vlr-commandEnded     . sh-on-command-end)
        (:vlr-commandCancelled . sh-on-command-end)
        (:vlr-commandFailed    . sh-on-command-end)
      )
    )
  )
  T
)
