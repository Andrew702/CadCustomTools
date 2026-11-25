;;; ==========================================================================
;;; src/sf_collect.lsp - Selection Grouping, Counting & Filtering
;;; ==========================================================================

;; Insert an entity into the matching (label count entList) bucket.
;; Walks only the outer list of distinct types (typically < 20) and prepends
;; the entity in O(1), avoiding O(N^2) list equality checks from (subst ...).
(defun sf-add-to-group (label ent groups / head rest found entry)
  (setq head  nil
        rest  groups
        found nil
  )
  (while (and rest (not found))
    (setq entry (car rest))
    (if (= (car entry) label)
      (setq head  (cons (list label (1+ (cadr entry)) (cons ent (caddr entry))) head)
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
    (cons (list label 1 (list ent)) groups)
  )
)

;; Group all entities in selection set `ss` by friendly object type label.
;; Returns (sortedGroups totalCount), where each group is (label count entList).
(defun sf-group-selection (ss / i n ent edata res label xrefCache groups)
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
                  groups    (sf-add-to-group label ent groups)
            )
          )
        )
        (setq i (1+ i))
      )
      (list (sf-sort-groups groups) n)
    )
  )
)

;; Build a new selection set containing only entities from checked groups.
;; `checkedFlags` is a list of "1" / "0" strings parallel to `groups`.
(defun sf-build-selection (groups checkedFlags / outSS g f)
  (setq outSS (ssadd))
  (while (and groups checkedFlags)
    (setq g (car groups)
          f (car checkedFlags)
    )
    (if (= f "1")
      (foreach ent (caddr g)
        (ssadd ent outSS)
      )
    )
    (setq groups       (cdr groups)
          checkedFlags (cdr checkedFlags)
    )
  )
  outSS
)

;; Format a concise command-line summary of the filtered selection.
(defun sf-format-summary (groups checkedFlags totalCount selectedCount / parts g f detail)
  (if (= selectedCount 0)
    (strcat "\nSmartFilter: 0 of " (itoa totalCount) " objects selected (selection cleared).")
    (progn
      (setq parts nil)
      (while (and groups checkedFlags)
        (setq g (car groups)
              f (car checkedFlags)
        )
        (if (= f "1")
          (setq parts (cons (strcat (car g) ": " (itoa (cadr g))) parts))
        )
        (setq groups       (cdr groups)
              checkedFlags (cdr checkedFlags)
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
