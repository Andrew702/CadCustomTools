;;; ==========================================================================
;;; src/sf_types.lsp - DXF Entity Type Classification & Sorting
;;; ==========================================================================

;; Check if a block definition is an Xref (DXF group 70 bit 4), using a
;; per-run cache alist ((UPPERNAME . T/nil) ...) to avoid repeated tblsearch.
;; Returns (isXref . updatedCache)
(defun sf-is-xref-block (blkName cache / key hit tbl flags isXref)
  (if (or (null blkName) (= blkName ""))
    (cons nil cache)
    (progn
      (setq key (strcase blkName)
            hit (assoc key cache)
      )
      (if hit
        (cons (cdr hit) cache)
        (progn
          (setq tbl    (tblsearch "BLOCK" blkName)
                flags  (if tbl (cdr (assoc 70 tbl)) 0)
                isXref (and (numberp flags) (> (logand flags 4) 0))
                cache  (cons (cons key isXref) cache)
          )
          (cons isXref cache)
        )
      )
    )
  )
)

;; Map an entity's entget data to a human-readable Revit-style type label.
;; Returns (label . updatedXrefCache)
(defun sf-entity-type-label (edata xrefCache / etype flags blkName res label)
  (setq etype (cdr (assoc 0 edata)))
  (cond
    ((null etype)
     (cons "Unknown" xrefCache)
    )
    ((= etype "INSERT")
     (setq blkName (cdr (assoc 2 edata))
           res     (sf-is-xref-block blkName xrefCache)
           label   (if (car res) "Xref" "Block Reference")
     )
     (cons label (cdr res))
    )
    ((= etype "POLYLINE")
     (setq flags (cdr (assoc 70 edata)))
     (if (not (numberp flags)) (setq flags 0))
     (setq label
       (cond
         ((> (logand flags 64) 0) "Polyface Mesh")
         ((> (logand flags 16) 0) "Polygon Mesh")
         ((> (logand flags 8) 0)  "3D Polyline")
         (T                       "Polyline")
       )
     )
     (cons label xrefCache)
    )
    (T
     (setq label
       (cond
         ((= etype "LINE")            "Line")
         ((= etype "LWPOLYLINE")      "Polyline")
         ((= etype "CIRCLE")          "Circle")
         ((= etype "ARC")             "Arc")
         ((= etype "ELLIPSE")         "Ellipse")
         ((= etype "SPLINE")          "Spline")
         ((= etype "POINT")           "Point")
         ((= etype "TEXT")            "Text")
         ((= etype "MTEXT")           "MText")
         ((= etype "ATTDEF")          "Attribute Definition")
         ((= etype "HATCH")           "Hatch")
         ((= etype "DIMENSION")       "Dimension")
         ((= etype "LEADER")          "Leader")
         ((or (= etype "MULTILEADER") (= etype "MLEADER")) "Multileader")
         ((= etype "WIPEOUT")         "Wipeout")
         ((= etype "SOLID")           "2D Solid")
         ((= etype "3DSOLID")         "3D Solid")
         ((= etype "3DFACE")          "3D Face")
         ((= etype "REGION")          "Region")
         ((= etype "IMAGE")           "Raster Image")
         ((= etype "PDFUNDERLAY")     "PDF Underlay")
         ((= etype "DWFUNDERLAY")     "DWF Underlay")
         ((= etype "DGNUNDERLAY")     "DGN Underlay")
         ((= etype "VIEWPORT")        "Viewport")
         ((= etype "ACAD_TABLE")      "Table")
         ((= etype "XLINE")           "Xline")
         ((= etype "RAY")             "Ray")
         ((= etype "MLINE")           "Multiline")
         ((= etype "TRACE")           "Trace")
         ((= etype "TOLERANCE")       "Tolerance")
         ((= etype "HELIX")           "Helix")
         ((= etype "BODY")            "Body")
         ((wcmatch etype "*SURFACE")  "Surface")
         (T                           etype)
       )
     )
     (cons label xrefCache)
    )
  )
)

;; Sort grouped list ((label count entList) ...) alphabetically by label
(defun sf-sort-groups (groups)
  (vl-sort groups
    (function
      (lambda (a b)
        (< (strcase (car a)) (strcase (car b)))
      )
    )
  )
)

(princ)
