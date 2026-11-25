;;; ==========================================================================
;;; HvacRoom/HvacRoom.lsp - Entry Point & Module Loader for HvacRoom (ZWCAD)
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

;; Helper: compute area/CHW/pipe, place MTEXT, print status, bump counter
(defun rt-place-room-tag (placePt rawArea / areaM2 chwEn chwGpm pipeStr roomName sep labelStr statusMsg mtextEnt)
  (setq areaM2   (rt-calc-area rawArea *rt-factor* *rt-bias* *rt-prec* *rt-round-mode*)
        chwEn    (= *rt-chw-en* "1")
        chwGpm   (if chwEn (rt-calc-chw areaM2 *rt-ton-factor* *rt-prec*) 0.0)
        pipeStr  (if (and chwEn (= *rt-pipe-en* "1"))
                   (rt-calc-pipe-size chwGpm *rt-pipe-min-idx* *rt-pipe-unit*)
                   nil)
        roomName (strcat *rt-rtype* "#" (itoa *rt-num*))
        sep      (if (= *rt-multiline* "1") "\\P" " ")
        labelStr (rt-format-label roomName areaM2 *rt-prec* *rt-extra* sep chwEn chwGpm pipeStr nil)
        mtextEnt (rt-create-room-mtext
                   placePt *rt-height* labelStr roomName *rt-rtype* areaM2
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
  mtextEnt
)

;; --------------------------------------------------------------------------
;; COMMAND: HVACROOM
;; --------------------------------------------------------------------------
(defun c:HVACROOM (/ *error* oldEcho keepLoop rc pickLoop pickMode inp res
                     ent selRes previewEnt previewOwned lastMtext)
  (defun *error* (msg)
    (if previewEnt (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned))
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
      (setq oldEcho      (getvar "CMDECHO")
            previewEnt   nil
            previewOwned nil
            lastMtext    nil
            pickMode     "point"
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
                 (setq inp (entsel (strcat "\nSelect closed polyline for " *rt-rtype* "#" (itoa *rt-num*) " [Pick/Undo/Dialog] <Done>: ")))
               )
               (progn
                 (initget "Select Undo Dialog")
                 (setq inp (getpoint (strcat "\nPick inside room for " *rt-rtype* "#" (itoa *rt-num*) " [Select/Undo/Dialog] <Done>: ")))
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
                        (rt-clear-preview previewEnt nil nil previewOwned)
                        (setq previewEnt   nil
                              previewOwned nil
                        )
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
               ((= inp "Dialog")
                (if previewEnt
                  (progn
                    (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned)
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
                (if (and (= pickMode "select") (= (getvar "ERRNO") 7))
                  (princ "\nNo object selected. Try again or press Enter to finish.")
                  (progn
                    (if previewEnt
                      (progn
                        (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned)
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
                    (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned)
                    (setq previewEnt   nil
                          previewOwned nil
                    )
                  )
                )
                (setq ent    (car inp)
                      selRes (rt-get-area-from-ent ent)
                )
                (if selRes
                  (progn
                    (setq previewEnt   ent
                          previewOwned nil
                    )
                    (redraw ent 3) ; Highlight selected polyline
                    (setq lastMtext (rt-place-room-tag (cadr selRes) (car selRes)))
                  )
                  (princ "\nSelected object has no closed area. Select a closed polyline, circle, region, or hatch.")
                )
               )
               ;; Point Pick Mode: getpoint returned (x y z)
               ((listp inp)
                (if previewEnt
                  (progn
                    (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned)
                    (setq previewEnt   nil
                          previewOwned nil
                    )
                  )
                )
                (setq res (rt-get-area-at inp (= *rt-fast-bndry* "1")))
                (if res
                  (progn
                    (setq previewEnt   (cadr res)
                          previewOwned T
                    )
                    (rt-show-preview previewEnt)
                    (setq lastMtext (rt-place-room-tag inp (car res)))
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
        (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned)
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