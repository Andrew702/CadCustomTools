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

;; Helper: compute area/sprinklers, place MTEXT, print status, bump counter
(defun ff-place-room-tag (placePt rawArea / areaM2 sprCount roomName sep labelStr mtextEnt)
  (setq areaM2   (ff-calc-area rawArea *ff-factor* *ff-bias* *ff-prec* *ff-round-mode*)
        sprCount (ff-calc-sprinklers areaM2 *ff-cov-area*)
        roomName (strcat *ff-rtype* "#" (itoa *ff-num*))
        sep      (if (= *ff-multiline* "1") "\\P" " ")
        labelStr (ff-format-label roomName areaM2 *ff-prec* sprCount *ff-extra* sep)
        mtextEnt (ff-create-room-mtext
                   placePt *ff-height* labelStr roomName *ff-rtype* areaM2
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
  mtextEnt
)

;; --------------------------------------------------------------------------
;; COMMAND: FFSPRINKLER
;; --------------------------------------------------------------------------
(defun c:FFSPRINKLER (/ *error* oldEcho keepLoop rc pickLoop pickMode inp res
                        ent selRes previewEnt previewOwned lastMtext)
  (defun *error* (msg)
    (if previewEnt (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer* previewOwned))
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
      (setq oldEcho      (getvar "CMDECHO")
            previewEnt   nil
            previewOwned nil
            lastMtext    nil
            pickMode     "point"
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
          ;; Append Sprinklers to existing MTEXT area tags
          ((= rc 3)
           (ff-append-sprinklers-to-selection)
           (setq keepLoop nil)
          )
          ;; Pick / Select Rooms
          ((= rc 1)
           (setq pickLoop  T
                 lastMtext nil
           )
           (while pickLoop
             (if (= pickMode "select")
               (progn
                 (setvar "ERRNO" 0)
                 (initget "Pick Undo Dialog")
                 (setq inp (entsel (strcat "\nSelect closed polyline for " *ff-rtype* "#" (itoa *ff-num*) " [Pick/Undo/Dialog] <Done>: ")))
               )
               (progn
                 (initget "Select Undo Dialog")
                 (setq inp (getpoint (strcat "\nPick inside room for " *ff-rtype* "#" (itoa *ff-num*) " [Select/Undo/Dialog] <Done>: ")))
               )
             )
             (cond
               ((= inp "Select")
                (setq pickMode "select")
               )
               ((= inp "Pick")
                (setq pickMode "point")
               )
               ((= inp "Undo")
                (if (and lastMtext (entget lastMtext))
                  (progn
                    (entdel lastMtext)
                    (setq lastMtext nil)
                    (if previewEnt
                      (progn
                        ;; Delete if we created it; only unhighlight if pre-existing
                        (ff-clear-preview previewEnt nil nil previewOwned)
                        (setq previewEnt   nil
                              previewOwned nil
                        )
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
               ((= inp "Dialog")
                (if previewEnt
                  (progn
                    (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer* previewOwned)
                    (setq previewEnt   nil
                          previewOwned nil
                    )
                  )
                )
                (setq lastMtext nil
                      pickLoop  nil
                )
               )
               ((null inp)
                ;; In select mode, ERRNO=7 means missed pick (keep looping); otherwise Done
                (if (and (= pickMode "select") (= (getvar "ERRNO") 7))
                  (princ "\nNo object selected. Try again or press Enter to finish.")
                  (progn
                    (if previewEnt
                      (progn
                        (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer* previewOwned)
                        (setq previewEnt   nil
                              previewOwned nil
                        )
                      )
                    )
                    (setq pickLoop nil
                          keepLoop nil
                    )
                  )
                )
               )
               ;; Select Mode: entsel returned (ename pickPt)
               ((and (= pickMode "select") (listp inp) (eq (type (car inp)) 'ENAME))
                (if previewEnt
                  (progn
                    (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer* previewOwned)
                    (setq previewEnt   nil
                          previewOwned nil
                    )
                  )
                )
                (setq ent    (car inp)
                      selRes (ff-get-area-from-ent ent)
                )
                (if selRes
                  (progn
                    (setq previewEnt   ent
                          previewOwned nil
                    )
                    (redraw ent 3) ; Highlight selected polyline
                    (setq lastMtext (ff-place-room-tag (cadr selRes) (car selRes)))
                  )
                  (princ "\nSelected object has no closed area. Select a closed polyline, circle, region, or hatch.")
                )
               )
               ;; Point Pick Mode: getpoint returned (x y z)
               ((listp inp)
                (if previewEnt
                  (progn
                    (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer* previewOwned)
                    (setq previewEnt   nil
                          previewOwned nil
                    )
                  )
                )
                (setq res (ff-get-area-at inp (= *ff-fast-bndry* "1")))
                (if res
                  (progn
                    (setq previewEnt   (cadr res)
                          previewOwned T
                    )
                    (ff-show-preview previewEnt)
                    (setq lastMtext (ff-place-room-tag inp (car res)))
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
        (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer* previewOwned)
      )
      (setvar "CMDECHO" oldEcho)
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMAND: FFAPPEND (direct shortcut to append sprinklers to selected tags)
;; --------------------------------------------------------------------------
(defun c:FFAPPEND ()
  (if (not *ff-loaded*)
    (setq *ff-loaded* (ff-load-modules))
  )
  (if *ff-loaded*
    (ff-append-sprinklers-to-selection)
    (princ "\nFireSprinkler modules are not loaded.")
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
  (princ "\nFireSprinkler loaded. Commands: FFSPRINKLER, FFAPPEND, FFEXPORT\n")
)
(princ)
