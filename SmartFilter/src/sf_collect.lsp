;;; ==========================================================================
;;; src/sf_collect.lsp - Selection Grouping (Type + Layer), Counting & Filtering
;;; ==========================================================================

;; Insert an entity into a (layerName count entList) bucket in O(L) with O(1) prepend.
(defun sf-add-to-layer-group (layerName ent layerGroups / head rest found entry)
  (setq head  nil
        rest  layerGroups
        found nil
  )
  (while (and rest (not found))
    (setq entry (car rest))
    (if (= (car entry) layerName)
      (setq head  (cons (list layerName (1+ (cadr entry)) (cons ent (caddr entry))) head)
            rest  (cdr rest)
            found T
      )
      (setq head (cons entry head)
            rest (cdr rest)
      )
    )
  )
  (if found
    (append (reverse head) rest)
    (cons (list layerName 1 (list ent)) layerGroups)
  )
)

;; Insert an entity into the matching (typeLabel totalCount layerGroups) bucket.
;; Walks only the outer list of distinct types (typically < 20) and prepends
;; the entity in O(1), avoiding O(N^2) list equality checks from (subst ...).
(defun sf-add-to-group (typeLabel layerName ent groups / head rest found entry)
  (setq head  nil
        rest  groups
        found nil
  )
  (while (and rest (not found))
    (setq entry (car rest))
    (if (= (car entry) typeLabel)
      (setq head  (cons (list typeLabel
                              (1+ (cadr entry))
                              (sf-add-to-layer-group layerName ent (caddr entry))
                        )
                        head
                  )
            rest  (cdr rest)
            found T
      )
      (setq head (cons entry head)
            rest (cdr rest)
      )
    )
  )
  (if found
    (append (reverse head) rest)
    (cons (list typeLabel 1 (list (list layerName 1 (list ent)))) groups)
  )
)

;; Group all entities in selection set `ss` by object type and layer.
;; Returns (sortedGroups totalCount), where each group is:
;;   (typeLabel typeCount ((layerName layerCount entList) ...))
(defun sf-group-selection (ss / i n ent edata res label layerName xrefCache groups sorted)
  (if (or (null ss) (= (sslength ss) 0))
    (list nil 0)
    (progn
      (setq n         (sslength ss)
            i         0
            xrefCache nil
            groups    nil
      )
      (while (< i n)
        (setq ent   (ssname ss i)
              edata (entget ent)
        )
        (if edata
          (progn
            (setq res       (sf-entity-type-label edata xrefCache)
                  label     (car res)
                  xrefCache (cdr res)
                  layerName (cdr (assoc 8 edata))
            )
            (if (or (null layerName) (= layerName ""))
              (setq layerName "0")
            )
            (setq groups (sf-add-to-group label layerName ent groups))
          )
        )
        (setq i (1+ i))
      )
      ;; Sort outer groups alphabetically by type label, and inner layers by layer name
      (setq sorted
        (mapcar
          (function
            (lambda (g)
              (list (car g) (cadr g) (sf-sort-groups (caddr g)))
            )
          )
          (sf-sort-groups groups)
        )
      )
      (list sorted n)
    )
  )
)

;; Build a new selection set containing only entities from checked type/layer buckets.
;; `checkedLayerFlags` is a list of lists of "1"/"0" strings parallel to each group's layerGroups.
(defun sf-build-selection (groups checkedLayerFlags / outSS g lFlags lGroups lg f)
  (setq outSS (ssadd))
  (while (and groups checkedLayerFlags)
    (setq g       (car groups)
          lFlags  (car checkedLayerFlags)
          lGroups (caddr g)
    )
    (while (and lGroups lFlags)
      (setq lg (car lGroups)
            f  (car lFlags)
      )
      (if (= f "1")
        (foreach ent (caddr lg)
          (ssadd ent outSS)
        )
      )
      (setq lGroups (cdr lGroups)
            lFlags  (cdr lFlags)
      )
    )
    (setq groups            (cdr groups)
          checkedLayerFlags (cdr checkedLayerFlags)
    )
  )
  outSS
)

;; Format a concise command-line summary of the filtered selection.
;; If a type is partially filtered by layer, includes the selected layer names.
(defun sf-format-summary (groups checkedLayerFlags totalCount selectedCount
                          / parts g lFlags lGroups lg f typeSel selLayers allLayers
                            layerStr detail)
  (if (= selectedCount 0)
    (strcat "\nSmartFilter: 0 of " (itoa totalCount) " objects selected (selection cleared).")
    (progn
      (setq parts nil)
      (while (and groups checkedLayerFlags)
        (setq g         (car groups)
              lFlags    (car checkedLayerFlags)
              lGroups   (caddr g)
              typeSel   0
              selLayers nil
              allLayers T
        )
        (while (and lGroups lFlags)
          (setq lg (car lGroups)
                f  (car lFlags)
          )
          (if (= f "1")
            (setq typeSel   (+ typeSel (cadr lg))
                  selLayers (cons (car lg) selLayers)
            )
            (setq allLayers nil)
          )
          (setq lGroups (cdr lGroups)
                lFlags  (cdr lFlags)
          )
        )
        (if (> typeSel 0)
          (if allLayers
            (setq parts (cons (strcat (car g) ": " (itoa typeSel)) parts))
            (progn
              (setq layerStr "")
              (foreach ln (reverse selLayers)
                (setq layerStr (if (= layerStr "") ln (strcat layerStr ", " ln)))
              )
              (setq parts (cons (strcat (car g) " [" layerStr "]: " (itoa typeSel)) parts))
            )
          )
        )
        (setq groups            (cdr groups)
              checkedLayerFlags (cdr checkedLayerFlags)
        )
      )
      (setq parts  (reverse parts)
            detail ""
      )
      (foreach p parts
        (setq detail (if (= detail "") p (strcat detail ", " p)))
      )
      (strcat "\nSmartFilter: "
              (itoa selectedCount) " of " (itoa totalCount)
              " objects selected (" detail ")."
      )
    )
  )
)

(princ)
