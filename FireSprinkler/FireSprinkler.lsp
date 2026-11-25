;;; ==========================================================================
;;; FireSprinkler/FireSprinkler.lsp - Entry Point & Loader (ZWCAD, NFPA 13)
;;; Commands: FFSPRINKLER, FFEXPORT
;;; ==========================================================================

(vl-load-com)

;; --------------------------------------------------------------------------
;; Module Loader
;; --------------------------------------------------------------------------

(defun ff-ensure-trailing-slash (dir)
  (if (and dir (> (strlen dir) 0))
    (if (member (substr dir (strlen dir) 1) '("\\" "/"))
      dir
      (strcat dir "/")
    )
    ""
  )
)

(defun ff-resolve-root-dir (/ p)
  (cond
    ((and *ff-root-dir* (findfile (strcat *ff-root-dir* "src/ff_state.lsp")))
     *ff-root-dir*
    )
    ((setq p (findfile "FireSprinkler.lsp"))
     (ff-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "FireSprinkler/FireSprinkler.lsp"))
     (ff-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "src/ff_state.lsp"))
     (ff-ensure-trailing-slash (vl-filename-directory (vl-filename-directory p)))
    )
    ((findfile (strcat (getvar "DWGPREFIX") "src/ff_state.lsp"))
     (ff-ensure-trailing-slash (getvar "DWGPREFIX"))
    )
    ((setq p (getfiled "Locate FireSprinkler.lsp (add folder to Support Path to skip)" "FireSprinkler.lsp" "lsp" 0))
     (ff-ensure-trailing-slash (vl-filename-directory p))
    )
    (T nil)
  )
)

(defun ff-load-modules (/ modules ok modPath)
  (setq *ff-root-dir* (ff-resolve-root-dir))
  (if (null *ff-root-dir*)
    (progn
      (princ "\nError: Could not locate FireSprinkler src/ modules.")
      nil
    )
    (progn
      (setq modules '("src/ff_state.lsp"
                      "src/ff_math.lsp"
                      "src/ff_cad.lsp"
                      "src/ff_dialog.lsp"
                      "src/ff_export.lsp")
            ok      T
      )
      (foreach m modules
        (setq modPath (strcat *ff-root-dir* m))
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

(setq *ff-loaded* (ff-load-modules))

;; --------------------------------------------------------------------------
;; COMMAND: FFSPRINKLER
;; --------------------------------------------------------------------------
(defun c:FFSPRINKLER (/ *error* oldEcho keepLoop rc pt res rawArea areaM2 sprCount
                        roomName sep labelStr previewEnt lastMtext)
  (defun *error* (msg)
    (if previewEnt (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer*))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (if (not *ff-loaded*)
    (setq *ff-loaded* (ff-load-modules))
  )

  (if (not *ff-loaded*)
    (princ "\nFireSprinkler could not start because required modules failed to load.")
    (progn
      (setq oldEcho    (getvar "CMDECHO")
            previewEnt nil
            lastMtext  nil
      )
      (setvar "CMDECHO" 0)
      (regapp "FFSPRINKLER_DATA")

      (setq keepLoop T)
      (while keepLoop
        (setq rc (ff-show-dialog))
        (cond
          ;; Cancel
          ((= rc 0)
           (setq keepLoop nil)
          )
          ;; Export Excel (CSV)
          ((= rc 2)
           (ff-do-export)
           (setq keepLoop nil)
          )
          ;; Pick Rooms
          ((= rc 1)
           (setq pt T
                 lastMtext nil
           )
           (while pt
             (initget "Undo Dialog")
             (setq pt (getpoint (strcat "\nPick inside room for " *ff-rtype* "#" (itoa *ff-num*) " [Undo/Dialog] <Done>: ")))
             (cond
               ((= pt "Undo")
                (if (and lastMtext (entget lastMtext))
                  (progn
                    (entdel lastMtext)
                    (setq lastMtext nil)
                    (if previewEnt
                      (progn
                        (ff-clear-preview previewEnt nil nil)
                        (setq previewEnt nil)
                      )
                    )
                    (if (> *ff-num* 0)
                      (setq *ff-num* (1- *ff-num*))
                    )
                    (ff-set-count *ff-rtype* (max 0 (1- *ff-num*)))
                    (princ (strcat "\nUndid last tag. Next room: " *ff-rtype* "#" (itoa *ff-num*)))
                  )
                  (princ "\nNothing to undo in this pick session.")
                )
               )
               ((= pt "Dialog")
                (if previewEnt
                  (progn
                    (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer*)
                    (setq previewEnt nil)
                  )
                )
                (setq lastMtext nil
                      pt        nil
                )
               )
               ((null pt)
                (if previewEnt
                  (progn
                    (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer*)
                    (setq previewEnt nil)
                  )
                )
                (setq keepLoop nil)
               )
               ((listp pt)
                (if previewEnt
                  (progn
                    (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer*)
                    (setq previewEnt nil)
                  )
                )
                (setq res (ff-get-area-at pt))
                (if res
                  (progn
                    (setq rawArea    (car res)
                          previewEnt (cadr res)
                    )
                    (ff-show-preview previewEnt)

                    (setq areaM2   (ff-calc-area rawArea *ff-factor* *ff-bias* *ff-prec* *ff-round-mode*))
                    (setq sprCount (ff-calc-sprinklers areaM2 *ff-cov-area*))
                    (setq roomName (strcat *ff-rtype* "#" (itoa *ff-num*)))
                    (setq sep      (if (= *ff-multiline* "1") "\\P" " "))
                    (setq labelStr (ff-format-label roomName areaM2 *ff-prec* sprCount *ff-extra* sep))

                    (setq lastMtext
                      (ff-create-room-mtext
                        pt *ff-height* labelStr roomName *ff-rtype* areaM2
                        *ff-hazard* *ff-cov-area* sprCount *ff-mtext-layer*
                      )
                    )

                    (princ
                      (strcat "\nPlaced: " roomName
                              " -> " (rtos areaM2 2 *ff-prec*) " m2"
                              " | " *ff-hazard*
                              " | Sprinklers: " (itoa sprCount))
                    )

                    (ff-set-count *ff-rtype* *ff-num*)
                    (setq *ff-num* (1+ *ff-num*))
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
        (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer*)
      )
      (setvar "CMDECHO" oldEcho)
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMAND: FFEXPORT (direct shortcut without opening dialog)
;; --------------------------------------------------------------------------
(defun c:FFEXPORT ()
  (if (not *ff-loaded*)
    (setq *ff-loaded* (ff-load-modules))
  )
  (if *ff-loaded*
    (ff-do-export)
    (princ "\nFireSprinkler modules are not loaded.")
  )
  (princ)
)

(if *ff-loaded*
  (princ "\nFireSprinkler loaded. Commands: FFSPRINKLER, FFEXPORT\n")
)
(princ)
