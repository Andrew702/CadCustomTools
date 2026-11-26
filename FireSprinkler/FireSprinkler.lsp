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
      (setq modules '("src/ff_log.lsp"
                      "src/ff_state.lsp"
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
      (if ff-log-event
        (ff-log-event (if ok "INFO" "ERROR") "MODULE_LOAD"
          (list
            (cons "root_dir" *ff-root-dir*)
            (cons "ok"       (ff-log-bool ok))
          )
        )
      )
      ok
    )
  )
)

(setq *ff-loaded* (ff-load-modules))

;; Helper: compute area/sprinklers, place MTEXT, print status, bump counter
(defun ff-place-room-tag (placePt rawArea / areaM2 sprCount roomName sep labelStr mtextEnt numBefore)
  (setq numBefore *ff-num*
        areaM2    (ff-calc-area rawArea *ff-factor* *ff-bias* *ff-prec* *ff-round-mode*)
        sprCount  (ff-calc-sprinklers areaM2 *ff-cov-area*)
        roomName  (strcat *ff-rtype* "#" (itoa *ff-num*))
        sep       (if (= *ff-multiline* "1") "\\P" " ")
        labelStr  (ff-format-label roomName areaM2 *ff-prec* sprCount *ff-extra* sep)
        mtextEnt  (ff-create-room-mtext
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
  (ff-log-event (if mtextEnt "INFO" "ERROR") "TAG_PLACED"
    (list
      (cons "room_name"    roomName)
      (cons "rtype"        *ff-rtype*)
      (cons "num_before"   numBefore)
      (cons "num_after"    *ff-num*)
      (cons "raw_area"     (float rawArea))
      (cons "factor"       (float *ff-factor*))
      (cons "bias"         (float *ff-bias*))
      (cons "round_mode"   *ff-round-mode*)
      (cons "prec"         *ff-prec*)
      (cons "area_m2"      (float areaM2))
      (cons "hazard"       *ff-hazard*)
      (cons "cov_area"     (float *ff-cov-area*))
      (cons "sprinklers"   sprCount)
      (cons "layer"        *ff-mtext-layer*)
      (cons "mtext_handle" (ff-log-handle mtextEnt))
      (cons "label"        labelStr)
    )
  )
  mtextEnt
)

;; --------------------------------------------------------------------------
;; COMMAND: FFSPRINKLER
;; --------------------------------------------------------------------------
(defun c:FFSPRINKLER (/ *error* oldEcho keepLoop rc pickLoop pickMode inp res
                        ent selRes previewEnt previewOwned lastMtext
                        placedCnt undoneCnt undoHandle numBefore outcome)
  (defun *error* (msg)
    (if previewEnt (ff-clear-preview previewEnt (= *ff-keepbndry* "1") *ff-bndry-layer* previewOwned))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if ff-log-error
      (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (ff-log-error msg)
        (ff-log-end "cancelled"
          (list
            (cons "placed_count" (if placedCnt placedCnt 0))
            (cons "undone_count" (if undoneCnt undoneCnt 0))
          )
        )
      )
    )
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
            placedCnt    0
            undoneCnt    0
            outcome      "done"
      )
      (setvar "CMDECHO" 0)
      (regapp "FFSPRINKLER_DATA")
      (ff-log-begin "FFSPRINKLER"
        (list
          (cons "rtype"    *ff-rtype*)
          (cons "hazard"   *ff-hazard*)
          (cons "cov_area" (float *ff-cov-area*))
        )
      )

      (setq keepLoop T)
      (while keepLoop
        (setq rc (ff-show-dialog))
        (ff-log-event "INFO" "DIALOG_CLOSE"
          (list
            (cons "rc"          rc)
            (cons "rtype"       *ff-rtype*)
            (cons "num"         *ff-num*)
            (cons "preset"      *ff-preset*)
            (cons "factor"      (float *ff-factor*))
            (cons "bias"        (float *ff-bias*))
            (cons "round_mode"  *ff-round-mode*)
            (cons "prec"        *ff-prec*)
            (cons "hazard"      *ff-hazard*)
            (cons "cov_area"    (float *ff-cov-area*))
            (cons "multiline"   (ff-log-bool (= *ff-multiline* "1")))
            (cons "keepbndry"   (ff-log-bool (= *ff-keepbndry* "1")))
            (cons "fast_bndry"  (ff-log-bool (= *ff-fast-bndry* "1")))
            (cons "mtext_layer" *ff-mtext-layer*)
            (cons "bndry_layer" *ff-bndry-layer*)
          )
        )
        (cond
          ;; Cancel
          ((= rc 0)
           (setq outcome  "cancelled"
                 keepLoop nil
           )
          )
          ;; Export Excel (CSV)
          ((= rc 2)
           (ff-do-export)
           (setq outcome  "exported"
                 keepLoop nil
           )
          )
          ;; Append Sprinklers to existing MTEXT area tags
          ((= rc 3)
           (ff-append-sprinklers-to-selection)
           (setq outcome  "appended"
                 keepLoop nil
           )
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
                (ff-log-event "INFO" "MODE_SWITCH" (list (cons "mode" "select")))
               )
               ((= inp "Pick")
                (setq pickMode "point")
                (ff-log-event "INFO" "MODE_SWITCH" (list (cons "mode" "point")))
               )
               ((= inp "Undo")
                (if (and lastMtext (entget lastMtext))
                  (progn
                    (setq undoHandle (ff-log-handle lastMtext)
                          numBefore  *ff-num*
                    )
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
                    (setq undoneCnt (1+ undoneCnt))
                    (ff-log-event "INFO" "TAG_UNDONE"
                      (list
                        (cons "undone_handle" undoHandle)
                        (cons "rtype"         *ff-rtype*)
                        (cons "num_before"    numBefore)
                        (cons "num_after"     *ff-num*)
                      )
                    )
                    (princ (strcat "\nUndid last tag. Next room: " *ff-rtype* "#" (itoa *ff-num*)))
                  )
                  (progn
                    (ff-log-event "INFO" "UNDO_EMPTY" nil)
                    (princ "\nNothing to undo in this pick session.")
                  )
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
                (ff-log-event (if selRes "INFO" "WARN") "BOUNDARY_DETECT"
                  (list
                    (cons "mode"       "select")
                    (cons "ent_handle" (ff-log-handle ent))
                    (cons "success"    (ff-log-bool selRes))
                    (cons "raw_area"   (if selRes (float (car selRes)) 0.0))
                  )
                )
                (if selRes
                  (progn
                    (setq previewEnt   ent
                          previewOwned nil
                    )
                    (redraw ent 3) ; Highlight selected polyline
                    (setq lastMtext (ff-place-room-tag (cadr selRes) (car selRes))
                          placedCnt (1+ placedCnt)
                    )
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
                (ff-log-event (if res "INFO" "WARN") "BOUNDARY_DETECT"
                  (list
                    (cons "mode"       "point")
                    (cons "fast_bndry" (ff-log-bool (= *ff-fast-bndry* "1")))
                    (cons "success"    (ff-log-bool res))
                    (cons "raw_area"   (if res (float (car res)) 0.0))
                    (cons "ent_handle" (if res (ff-log-handle (cadr res)) nil))
                  )
                )
                (if res
                  (progn
                    (setq previewEnt   (cadr res)
                          previewOwned T
                    )
                    (ff-show-preview previewEnt)
                    (setq lastMtext (ff-place-room-tag inp (car res))
                          placedCnt (1+ placedCnt)
                    )
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
      (ff-log-end outcome
        (list
          (cons "placed_count" placedCnt)
          (cons "undone_count" undoneCnt)
        )
      )
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMAND: FFAPPEND (direct shortcut to append sprinklers to selected tags)
;; --------------------------------------------------------------------------
(defun c:FFAPPEND (/ *error*)
  (defun *error* (msg)
    (if ff-log-error
      (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (ff-log-error msg)
        (ff-log-end "cancelled" nil)
      )
    )
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (if (not *ff-loaded*)
    (setq *ff-loaded* (ff-load-modules))
  )
  (if *ff-loaded*
    (progn
      (ff-log-begin "FFAPPEND"
        (list
          (cons "hazard"   *ff-hazard*)
          (cons "cov_area" (float *ff-cov-area*))
        )
      )
      (ff-append-sprinklers-to-selection)
      (ff-log-end "done" nil)
    )
    (princ "\nFireSprinkler modules are not loaded.")
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMAND: FFEXPORT (direct shortcut without opening dialog)
;; --------------------------------------------------------------------------
(defun c:FFEXPORT (/ *error*)
  (defun *error* (msg)
    (if ff-log-error
      (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (ff-log-error msg)
        (ff-log-end "cancelled" nil)
      )
    )
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (if (not *ff-loaded*)
    (setq *ff-loaded* (ff-load-modules))
  )
  (if *ff-loaded*
    (progn
      (ff-log-begin "FFEXPORT" nil)
      (ff-do-export)
      (ff-log-end "done" nil)
    )
    (princ "\nFireSprinkler modules are not loaded.")
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMAND: FFDEBUG (toggle / inspect FireSprinkler debug logger)
;; --------------------------------------------------------------------------
(defun c:FFDEBUG ()
  (if (not *ff-loaded*)
    (setq *ff-loaded* (ff-load-modules))
  )
  (if *ff-loaded*
    (ff-log-debug-cmd)
    (princ "\nFireSprinkler modules are not loaded.")
  )
  (princ)
)

(if *ff-loaded*
  (princ "\nFireSprinkler loaded. Commands: FFSPRINKLER, FFAPPEND, FFEXPORT, FFDEBUG\n")
)
(princ)
