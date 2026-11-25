;;; ==========================================================================
;;; HvacRoom.lsp - Entry Point & Module Loader for HvacRoom (ZWCAD)
;;; Commands: HVACROOM, HVACROOMEXPORT
;;; ==========================================================================

(vl-load-com)

;; --------------------------------------------------------------------------
;; Module Loader
;; --------------------------------------------------------------------------

(defun rt-ensure-trailing-slash (dir)
  (if (and dir (> (strlen dir) 0))
    (if (member (substr dir (strlen dir) 1) '("\\" "/"))
      dir
      (strcat dir "/")
    )
    ""
  )
)

(defun rt-resolve-root-dir (/ p)
  (cond
    ((and *rt-root-dir* (findfile (strcat *rt-root-dir* "src/rt_state.lsp")))
     *rt-root-dir*
    )
    ((setq p (findfile "HvacRoom.lsp"))
     (rt-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "HvacRoom/HvacRoom.lsp"))
     (rt-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "src/rt_state.lsp"))
     (rt-ensure-trailing-slash (vl-filename-directory (vl-filename-directory p)))
    )
    ((findfile (strcat (getvar "DWGPREFIX") "src/rt_state.lsp"))
     (rt-ensure-trailing-slash (getvar "DWGPREFIX"))
    )
    ((setq p (getfiled "Locate HvacRoom.lsp (add folder to Support Path to skip)" "HvacRoom.lsp" "lsp" 0))
     (rt-ensure-trailing-slash (vl-filename-directory p))
    )
    (T nil)
  )
)

(defun rt-load-modules (/ modules ok modPath)
  (setq *rt-root-dir* (rt-resolve-root-dir))
  (if (null *rt-root-dir*)
    (progn
      (princ "\nError: Could not locate HvacRoom src/ modules.")
      nil
    )
    (progn
      (setq modules '("src/rt_state.lsp"
                      "src/rt_math.lsp"
                      "src/rt_cad.lsp"
                      "src/rt_dialog.lsp"
                      "src/rt_export.lsp")
            ok      T
      )
      (foreach m modules
        (setq modPath (strcat *rt-root-dir* m))
        (if (findfile modPath)
          (load modPath)
          (progn
            (princ (strcat "\nError: Missing module: " modPath))
            (setq ok nil)
          )
        )
      )
      ok
    )
  )
)

(setq *rt-loaded* (rt-load-modules))

;; --------------------------------------------------------------------------
;; COMMAND: HVACROOM
;; --------------------------------------------------------------------------
(defun c:HVACROOM (/ *error* oldEcho keepLoop rc pt res rawArea areaM2 chwEn chwGpm
                     pipeStr roomName sep labelStr statusMsg previewEnt lastMtext)
  (defun *error* (msg)
    (if previewEnt (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer*))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (if (not *rt-loaded*)
    (setq *rt-loaded* (rt-load-modules))
  )

  (if (not *rt-loaded*)
    (princ "\nHvacRoom could not start because required modules failed to load.")
    (progn
      (setq oldEcho    (getvar "CMDECHO")
            previewEnt nil
            lastMtext  nil
      )
      (setvar "CMDECHO" 0)
      (regapp "ROOMTAG_DATA")

      (setq keepLoop T)
      (while keepLoop
        (setq rc (rt-show-dialog))
        (cond
          ;; Cancel
          ((= rc 0)
           (setq keepLoop nil)
          )
          ;; Export Excel (CSV)
          ((= rc 2)
           (rt-do-export)
           (setq keepLoop nil)
          )
          ;; Pick Rooms
          ((= rc 1)
           (setq pt T
                 lastMtext nil
           )
           (while pt
             (initget "Undo Dialog")
             (setq pt (getpoint (strcat "\nPick inside room for " *rt-rtype* "#" (itoa *rt-num*) " [Undo/Dialog] <Done>: ")))
             (cond
               ((= pt "Undo")
                (if (and lastMtext (entget lastMtext))
                  (progn
                    (entdel lastMtext)
                    (setq lastMtext nil)
                    (if previewEnt
                      (progn
                        (rt-clear-preview previewEnt nil nil) ; Always delete undone boundary
                        (setq previewEnt nil)
                      )
                    )
                    (if (> *rt-num* 0)
                      (setq *rt-num* (1- *rt-num*))
                    )
                    (rt-set-count *rt-rtype* (max 0 (1- *rt-num*)))
                    (princ (strcat "\nUndid last tag. Next room: " *rt-rtype* "#" (itoa *rt-num*)))
                  )
                  (princ "\nNothing to undo in this pick session.")
                )
               )
               ((= pt "Dialog")
                (if previewEnt
                  (progn
                    (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer*)
                    (setq previewEnt nil)
                  )
                )
                (setq lastMtext nil
                      pt        nil
                ) ; Re-open modal dialog
               )
               ((null pt)
                (if previewEnt
                  (progn
                    (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer*)
                    (setq previewEnt nil)
                  )
                )
                (setq keepLoop nil) ; Finish command
               )
               ((listp pt)
                ;; Finalize/clear previous room's preview before detecting next room
                (if previewEnt
                  (progn
                    (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer*)
                    (setq previewEnt nil)
                  )
                )
                (setq res (rt-get-area-at pt))
                (if res
                  (progn
                    (setq rawArea    (car res)
                          previewEnt (cadr res)
                    )
                    ;; Highlight detected boundary polyline as quick preview
                    (rt-show-preview previewEnt)

                    (setq areaM2   (rt-calc-area rawArea *rt-factor* *rt-bias* *rt-prec* *rt-round-mode*))
                    (setq chwEn    (= *rt-chw-en* "1"))
                    (setq chwGpm   (if chwEn (rt-calc-chw areaM2 *rt-ton-factor* *rt-prec*) 0.0))
                    (setq pipeStr  (if (and chwEn (= *rt-pipe-en* "1"))
                                     (rt-calc-pipe-size chwGpm *rt-pipe-min-idx* *rt-pipe-unit*)
                                     nil))
                    (setq roomName (strcat *rt-rtype* "#" (itoa *rt-num*)))
                    (setq sep      (if (= *rt-multiline* "1") "\\P" " "))
                    (setq labelStr (rt-format-label roomName areaM2 *rt-prec* *rt-extra* sep chwEn chwGpm pipeStr nil))

                    (setq lastMtext
                      (rt-create-room-mtext
                        pt *rt-height* labelStr roomName *rt-rtype* areaM2
                        chwEn chwGpm *rt-ton-factor* pipeStr *rt-mtext-layer*
                      )
                    )

                    (setq statusMsg (strcat "\nPlaced: " roomName " -> " (rtos areaM2 2 *rt-prec*) " m2"))
                    (if chwEn
                      (setq statusMsg (strcat statusMsg " | CHW: " (rtos chwGpm 2 *rt-prec*) " gpm"))
                    )
                    (if pipeStr
                      (setq statusMsg (strcat statusMsg " | phi " pipeStr))
                    )
                    (princ statusMsg)

                    (rt-set-count *rt-rtype* *rt-num*)
                    (setq *rt-num* (1+ *rt-num*))
                  )
                  (princ "\nCould not detect a closed boundary. Ensure the full room is visible and has no gaps.")
                )
               )
             )
           )
          )
        )
      )

      (if previewEnt
        (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer*)
      )
      (setvar "CMDECHO" oldEcho)
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMAND: HVACROOMEXPORT (direct shortcut without opening dialog)
;; --------------------------------------------------------------------------
(defun c:HVACROOMEXPORT ()
  (if (not *rt-loaded*)
    (setq *rt-loaded* (rt-load-modules))
  )
  (if *rt-loaded*
    (rt-do-export)
    (princ "\nHvacRoom modules are not loaded.")
  )
  (princ)
)

(if *rt-loaded*
  (princ "\nHvacRoom loaded. Commands: HVACROOM, HVACROOMEXPORT\n")
)
(princ)