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
      (setq modules '("src/rt_log.lsp"
                      "src/rt_state.lsp"
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
      (if rt-log-event
        (rt-log-event (if ok "INFO" "ERROR") "MODULE_LOAD"
          (list
            (cons "root_dir" *rt-root-dir*)
            (cons "ok"       (rt-log-bool ok))
          )
        )
      )
      ok
    )
  )
)

(setq *rt-loaded* (rt-load-modules))

;; Helper: compute area/CHW/pipe, place MTEXT, print status, bump counter
(defun rt-place-room-tag (placePt rawArea / areaM2 chwEn chwGpm pipeEn pipeStr roomName sep
                                            labelStr statusMsg mtextEnt numBefore)
  (setq numBefore *rt-num*
        areaM2    (rt-calc-area rawArea *rt-factor* *rt-bias* *rt-prec* *rt-round-mode*)
        chwEn     (= *rt-chw-en* "1")
        chwGpm    (if chwEn (rt-calc-chw areaM2 *rt-ton-factor* *rt-prec*) 0.0)
        pipeEn    (and chwEn (= *rt-pipe-en* "1"))
        pipeStr   (if pipeEn
                    (rt-calc-pipe-size chwGpm *rt-pipe-min-idx* *rt-pipe-unit*)
                    nil)
        roomName  (strcat *rt-rtype* "#" (itoa *rt-num*))
        sep       (if (= *rt-multiline* "1") "\\P" " ")
        labelStr  (rt-format-label roomName areaM2 *rt-prec* *rt-extra* sep chwEn chwGpm pipeStr nil)
        mtextEnt  (rt-create-room-mtext
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
  (rt-log-event (if mtextEnt "INFO" "ERROR") "TAG_PLACED"
    (list
      (cons "room_name"    roomName)
      (cons "rtype"        *rt-rtype*)
      (cons "num_before"   numBefore)
      (cons "num_after"    *rt-num*)
      (cons "raw_area"     (float rawArea))
      (cons "factor"       (float *rt-factor*))
      (cons "bias"         (float *rt-bias*))
      (cons "round_mode"   *rt-round-mode*)
      (cons "prec"         *rt-prec*)
      (cons "area_m2"      (float areaM2))
      (cons "chw_en"       (rt-log-bool chwEn))
      (cons "ton_factor"   (float *rt-ton-factor*))
      (cons "chw_gpm"      (float chwGpm))
      (cons "pipe_en"      (rt-log-bool pipeEn))
      (cons "pipe_unit"    *rt-pipe-unit*)
      (cons "pipe_min_idx" *rt-pipe-min-idx*)
      (cons "pipe_size"    (if pipeStr pipeStr ""))
      (cons "layer"        *rt-mtext-layer*)
      (cons "mtext_handle" (rt-log-handle mtextEnt))
      (cons "label"        labelStr)
    )
  )
  mtextEnt
)

;; --------------------------------------------------------------------------
;; COMMAND: HVACROOM
;; --------------------------------------------------------------------------
(defun c:HVACROOM (/ *error* oldEcho keepLoop rc pickLoop pickMode inp res
                     ent selRes previewEnt previewOwned lastMtext
                     placedCnt undoneCnt undoHandle numBefore outcome)
  (defun *error* (msg)
    (if previewEnt (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if rt-log-error
      (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (rt-log-error msg)
        (rt-log-end "cancelled"
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
            placedCnt    0
            undoneCnt    0
            outcome      "done"
      )
      (setvar "CMDECHO" 0)
      (regapp "ROOMTAG_DATA")
      (rt-log-begin "HVACROOM"
        (list
          (cons "rtype"  *rt-rtype*)
          (cons "preset" *rt-preset*)
          (cons "factor" (float *rt-factor*))
        )
      )

      (setq keepLoop T)
      (while keepLoop
        (setq rc (rt-show-dialog))
        (rt-log-event "INFO" "DIALOG_CLOSE"
          (list
            (cons "rc"           rc)
            (cons "rtype"        *rt-rtype*)
            (cons "num"          *rt-num*)
            (cons "preset"       *rt-preset*)
            (cons "factor"       (float *rt-factor*))
            (cons "bias"         (float *rt-bias*))
            (cons "round_mode"   *rt-round-mode*)
            (cons "prec"         *rt-prec*)
            (cons "chw_en"       (rt-log-bool (= *rt-chw-en* "1")))
            (cons "ton_factor"   (float *rt-ton-factor*))
            (cons "pipe_en"      (rt-log-bool (= *rt-pipe-en* "1")))
            (cons "pipe_unit"    *rt-pipe-unit*)
            (cons "pipe_min_idx" *rt-pipe-min-idx*)
            (cons "multiline"    (rt-log-bool (= *rt-multiline* "1")))
            (cons "keepbndry"    (rt-log-bool (= *rt-keepbndry* "1")))
            (cons "fast_bndry"   (rt-log-bool (= *rt-fast-bndry* "1")))
            (cons "mtext_layer"  *rt-mtext-layer*)
            (cons "bndry_layer"  *rt-bndry-layer*)
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
           (rt-do-export)
           (setq outcome  "exported"
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
                (rt-log-event "INFO" "MODE_SWITCH" (list (cons "mode" "select")))
               )
               ((= inp "Pick")
                (setq pickMode "point")
                (rt-log-event "INFO" "MODE_SWITCH" (list (cons "mode" "point")))
               )
               ((= inp "Undo")
                (if (and lastMtext (entget lastMtext))
                  (progn
                    (setq undoHandle (rt-log-handle lastMtext)
                          numBefore  *rt-num*
                    )
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
                    (setq undoneCnt (1+ undoneCnt))
                    (rt-log-event "INFO" "TAG_UNDONE"
                      (list
                        (cons "undone_handle" undoHandle)
                        (cons "rtype"         *rt-rtype*)
                        (cons "num_before"    numBefore)
                        (cons "num_after"     *rt-num*)
                      )
                    )
                    (princ (strcat "\nUndid last tag. Next room: " *rt-rtype* "#" (itoa *rt-num*)))
                  )
                  (progn
                    (rt-log-event "INFO" "UNDO_EMPTY" nil)
                    (princ "\nNothing to undo in this pick session.")
                  )
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
                (rt-log-event (if selRes "INFO" "WARN") "BOUNDARY_DETECT"
                  (list
                    (cons "mode"       "select")
                    (cons "ent_handle" (rt-log-handle ent))
                    (cons "success"    (rt-log-bool selRes))
                    (cons "raw_area"   (if selRes (float (car selRes)) 0.0))
                  )
                )
                (if selRes
                  (progn
                    (setq previewEnt   ent
                          previewOwned nil
                    )
                    (redraw ent 3) ; Highlight selected polyline
                    (setq lastMtext (rt-place-room-tag (cadr selRes) (car selRes))
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
                    (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned)
                    (setq previewEnt   nil
                          previewOwned nil
                    )
                  )
                )
                (setq res (rt-get-area-at inp (= *rt-fast-bndry* "1")))
                (rt-log-event (if res "INFO" "WARN") "BOUNDARY_DETECT"
                  (list
                    (cons "mode"       "point")
                    (cons "fast_bndry" (rt-log-bool (= *rt-fast-bndry* "1")))
                    (cons "success"    (rt-log-bool res))
                    (cons "raw_area"   (if res (float (car res)) 0.0))
                    (cons "ent_handle" (if res (rt-log-handle (cadr res)) nil))
                  )
                )
                (if res
                  (progn
                    (setq previewEnt   (cadr res)
                          previewOwned T
                    )
                    (rt-show-preview previewEnt)
                    (setq lastMtext (rt-place-room-tag inp (car res))
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
        (rt-clear-preview previewEnt (= *rt-keepbndry* "1") *rt-bndry-layer* previewOwned)
      )
      (setvar "CMDECHO" oldEcho)
      (rt-log-end outcome
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
;; COMMAND: HVACROOMEXPORT (direct shortcut without opening dialog)
;; --------------------------------------------------------------------------
(defun c:HVACROOMEXPORT (/ *error*)
  (defun *error* (msg)
    (if rt-log-error
      (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
        (rt-log-error msg)
        (rt-log-end "cancelled" nil)
      )
    )
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (if (not *rt-loaded*)
    (setq *rt-loaded* (rt-load-modules))
  )
  (if *rt-loaded*
    (progn
      (rt-log-begin "HVACROOMEXPORT" nil)
      (rt-do-export)
      (rt-log-end "done" nil)
    )
    (princ "\nHvacRoom modules are not loaded.")
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMAND: RTDEBUG (toggle / inspect HvacRoom debug logger)
;; --------------------------------------------------------------------------
(defun c:RTDEBUG ()
  (if (not *rt-loaded*)
    (setq *rt-loaded* (rt-load-modules))
  )
  (if *rt-loaded*
    (rt-log-debug-cmd)
    (princ "\nHvacRoom modules are not loaded.")
  )
  (princ)
)

(if *rt-loaded*
  (princ "\nHvacRoom loaded. Commands: HVACROOM, HVACROOMEXPORT, RTDEBUG\n")
)
(princ)