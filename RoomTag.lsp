;;; ==========================================================================
;;; RoomTag.lsp - Entry Point & Module Loader for RoomTag v2 (ZWCAD)
;;; Commands: ROOMTAG, ROOMEXPORT
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
    ((setq p (findfile "RoomTag.lsp"))
     (rt-ensure-trailing-slash (vl-filename-directory p))
    )
    ((setq p (findfile "src/rt_state.lsp"))
     (rt-ensure-trailing-slash (vl-filename-directory (vl-filename-directory p)))
    )
    ((findfile (strcat (getvar "DWGPREFIX") "src/rt_state.lsp"))
     (rt-ensure-trailing-slash (getvar "DWGPREFIX"))
    )
    ((setq p (getfiled "Locate RoomTag.lsp (add folder to Support Path to skip)" "RoomTag.lsp" "lsp" 0))
     (rt-ensure-trailing-slash (vl-filename-directory p))
    )
    (T nil)
  )
)

(defun rt-load-modules (/ modules ok modPath)
  (setq *rt-root-dir* (rt-resolve-root-dir))
  (if (null *rt-root-dir*)
    (progn
      (princ "\nError: Could not locate RoomTag src/ modules.")
      nil
    )
    (progn
      (setq modules '("src/rt_state.lsp"
                      "src/rt_math.lsp"
                      "src/rt_dialog.lsp"
                      "src/rt_cad.lsp"
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
;; COMMAND: ROOMTAG
;; --------------------------------------------------------------------------
(defun c:ROOMTAG (/ *error* oldEcho keepLoop rc pt rawArea areaM2 chwEn chwGpm
                    roomName sep labelStr statusMsg)
  (defun *error* (msg)
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
    (princ "\nRoomTag could not start because required modules failed to load.")
    (progn
      (setq oldEcho (getvar "CMDECHO"))
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
           (setq pt T)
           (while pt
             (initget "Dialog")
             (setq pt (getpoint (strcat "\nPick inside room for " *rt-rtype* "#" (itoa *rt-num*) " [Dialog] <Done>: ")))
             (cond
               ((= pt "Dialog")
                (setq pt nil) ; Re-open modal dialog
               )
               ((null pt)
                (setq keepLoop nil) ; Finish command
               )
               ((listp pt)
                (setq rawArea (rt-get-area-at pt (= *rt-keepbndry* "1")))
                (if rawArea
                  (progn
                    (setq areaM2 (rt-calc-area rawArea *rt-factor* *rt-bias* *rt-prec* *rt-round-mode*))
                    (setq chwEn  (= *rt-chw-en* "1"))
                    (setq chwGpm (if chwEn (rt-calc-chw areaM2 *rt-ton-factor* *rt-prec*) 0.0))
                    (setq roomName (strcat *rt-rtype* "#" (itoa *rt-num*)))
                    (setq sep      (if (= *rt-multiline* "1") "\\P" " "))
                    (setq labelStr (rt-format-label roomName areaM2 *rt-prec* *rt-extra* sep chwEn chwGpm))

                    (rt-create-room-mtext
                      pt *rt-height* labelStr roomName *rt-rtype* areaM2 chwEn chwGpm *rt-ton-factor*
                    )

                    (setq statusMsg (strcat "\nPlaced: " roomName " -> " (rtos areaM2 2 *rt-prec*) " m2"))
                    (if chwEn
                      (setq statusMsg (strcat statusMsg " | CHW: " (rtos chwGpm 2 *rt-prec*) " gpm"))
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

      (setvar "CMDECHO" oldEcho)
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; COMMAND: ROOMEXPORT (direct shortcut without opening dialog)
;; --------------------------------------------------------------------------
(defun c:ROOMEXPORT ()
  (if (not *rt-loaded*)
    (setq *rt-loaded* (rt-load-modules))
  )
  (if *rt-loaded*
    (rt-do-export)
    (princ "\nRoomTag modules are not loaded.")
  )
  (princ)
)

(if *rt-loaded*
  (princ "\nRoomTag v2 loaded. Commands: ROOMTAG, ROOMEXPORT\n")
)
(princ)