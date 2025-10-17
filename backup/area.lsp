;; ===============================================================================
;; Pure AutoLISP Area Calculator (DXF-based)
;; Command: SA (Simple Area)
;; Function: Calculate area for closed LWPOLYLINE (shoelace) and CIRCLE only
;; Notes: No VLA/ActiveX. DXF parsing only. Others → "Unable to calculate" Guide.
;; ===============================================================================

;; --- Helpers (pure AutoLISP, no VL- functions) ---------------------------------

(defun dxf-get (code elst)
  (cdr (assoc code elst))
)

(defun dxf-filter-values (code elst / r)
  ;; Collect all values for repeated DXF group code
  (setq r '())
  (while elst
    (if (= (caar elst) code)
      (setq r (cons (cdar elst) r))
    )
    (setq elst (cdr elst))
  )
  (reverse r)
)

(setq *areat-eps* 1e-6)

(defun areat-eps ()
  (if (and *areat-eps* (numberp *areat-eps*)) *areat-eps* 1e-6)
)

(defun nearly-equal-pt2d (p1 p2 / eps)
  (setq eps (areat-eps))
  (and (<= (abs (- (car p1) (car p2))) eps)
       (<= (abs (- (cadr p1) (cadr p2))) eps))
)

(defun c:areatset (/ v)
  (initget 6)
  (setq v (getreal (strcat "\nSet connection tolerance eps <current=" (rtos (areat-eps) 2 6) ">: ")))
  (if v (setq *areat-eps* v))
  (princ (strcat "\n[AREAT] eps set to " (rtos (areat-eps) 2 6)))
  (princ)
)

(defun nth-or (i lst def)
  (if (and (numberp i) (>= i 0) (< i (length lst)))
    (nth i lst)
    def
  )
)

(defun ensure-closed-pts (pts / last-pt)
  (setq last-pt (last-elt pts))
  (if (and pts (not (nearly-equal-pt2d (car pts) last-pt)))
    (append pts (list (car pts)))
    pts
  )
)

(defun last-elt (lst)
  (if (and lst (cdr lst))
    (last-elt (cdr lst))
    (car lst)
  )
)

(defun lwpoly-vertices-2d (elst / xs ys pts i n xpend scan v)
  ;; Supports three patterns:
  ;; A) Separate 10 (x) and 20 (y)
  ;; B) 10 holds point list (x y [z]); 20 absent
  ;; C) Sequential scan pairing 10 then 20
  (setq xs (dxf-filter-values 10 elst)
        ys (dxf-filter-values 20 elst)
        pts '())
  ;; Pattern B: (10 . (x y [z]))
  (if (and xs (listp (car xs)))
    (progn
      (setq i 0 n (length xs))
      (while (< i n)
        (setq v (nth i xs))
        (if (and (listp v) (>= (length v) 2))
          (setq pts (append pts (list (list (car v) (cadr v)))))
        )
        (setq i (1+ i))
      )
    )
    (progn
      ;; Pattern A: separate lists
      (setq i 0 n (min (length xs) (length ys)))
      (while (< i n)
        (setq pts (append pts (list (list (nth i xs) (nth i ys)))))
        (setq i (1+ i))
      )
      ;; Pattern C fallback if still insufficient
      (if (< (length pts) 3)
        (progn
          (setq scan elst
                pts '()
                xpend nil)
          (while scan
            (cond
              ((= (caar scan) 10) (setq xpend (cdar scan)))
              ((= (caar scan) 20)
               (if xpend
                 (progn
                   (setq pts (append pts (list (list xpend (cdar scan)))))
                   (setq xpend nil)
                 )
               )
              )
            )
            (setq scan (cdr scan))
          )
        )
      )
    )
  )
  pts
)

(defun polyline-vertices-2d (e / elst pts next vtype x y z bulge has3d hasBulge)
  ;; Traverse legacy POLYLINE/SEQEND, collect 2D vertices only; signal flags
  (setq elst (entget e)
        pts '()
        has3d nil
        hasBulge nil)
  (if (= (dxf-get 0 elst) "POLYLINE")
    (progn
      (setq next (entnext e))
      (while next
        (setq elst (entget next))
        (setq vtype (dxf-get 0 elst))
        (cond
          ((= vtype "VERTEX")
           (setq x (dxf-get 10 elst)
                 y (dxf-get 20 elst)
                 z (dxf-get 30 elst)
                 bulge (dxf-get 42 elst))
           (if (and (numberp z) (/= z 0.0)) (setq has3d T))
           (if (and (numberp bulge) (/= bulge 0.0)) (setq hasBulge T))
           (if (and (numberp x) (numberp y))
             (setq pts (append pts (list (list x y))))
           )
          )
          ((= vtype "SEQEND") (setq next nil))
        )
        (if next (setq next (entnext next)))
      )
    )
  )
  (list pts has3d hasBulge)
)

(defun polygon-area-shoelace (pts / s i n a b)
  ;; pts must be closed or we will close it virtually
  (setq pts (ensure-closed-pts pts))
  (setq s 0.0
        i 0
        n (1- (length pts))) ;; last equals first
  (while (< i n)
    (setq a (nth i pts)
          b (nth (1+ i) pts))
    (setq s (+ s (- (* (car a) (cadr b)) (* (car b) (cadr a)))))
    (setq i (1+ i))
  )
  (abs (/ s 2.0))
)

(defun segment-arc-wedge-area (p1 p2 bulge / s c theta r)
  ;; Returns signed area between chord p1->p2 and arc defined by bulge
  (if (and bulge (/= bulge 0.0))
    (progn
      (setq s bulge)
      (setq c (distance p1 p2))
      (setq theta (* 4.0 (atan s)))
      (if (= 0.0 s)
        0.0
        (progn
          ;; radius positive, sign carried by theta
          (setq r (/ (* c (+ 1.0 (* s s))) (* 4.0 (abs s))))
          (* 0.5 (* r r) (- theta (sin theta)))
        )
      )
    )
    0.0
  )
)

(defun lwpoly-area-from-elst (elst / pts bulges n i p1 p2 area chordArea wedgeArea nextIdx b)
  (setq pts (lwpoly-vertices-2d elst))
  (if (< (length pts) 3)
    nil
    (progn
      ;; check closed
      (if (not (is-lwpoly-closed elst pts))
        nil
        (progn
          ;; chordal polygon area
          (setq chordArea (polygon-area-shoelace pts))
          ;; bulges per-vertex; default 0. length may be < length pts
          (setq bulges (dxf-filter-values 42 elst))
          ;; ensure closure without duplicating last
          (setq n (length pts))
          (setq wedgeArea 0.0
                i 0)
          (while (< i n)
            (setq p1 (nth i pts)
                  nextIdx (if (< (1+ i) n) (1+ i) 0)
                  p2 (nth nextIdx pts)
                  b  (nth-or i bulges 0.0))
            (setq wedgeArea (+ wedgeArea (segment-arc-wedge-area p1 p2 b)))
            (setq i (1+ i))
          )
          (+ chordArea wedgeArea)
        )
      )
    )
  )
)

(defun polyline-vertices-2d-with-bulges (e / elst pts bulges next vtype x y z b has3d)
  (setq elst (entget e)
        pts '()
        bulges '()
        has3d nil)
  (if (= (dxf-get 0 elst) "POLYLINE")
    (progn
      (setq next (entnext e))
      (while next
        (setq elst (entget next))
        (setq vtype (dxf-get 0 elst))
        (cond
          ((= vtype "VERTEX")
           (setq x (dxf-get 10 elst)
                 y (dxf-get 20 elst)
                 z (dxf-get 30 elst)
                 b (dxf-get 42 elst))
           (if (and (numberp z) (/= z 0.0)) (setq has3d T))
           (if (and (numberp x) (numberp y))
             (progn
               (setq pts (append pts (list (list x y))))
               (if (numberp b)
                 (setq bulges (append bulges (list b)))
                 (setq bulges (append bulges (list 0.0)))
               )
             )
           )
          )
        (if (= vtype "SEQEND") (setq next nil) (setq next (entnext next)))
      )
    )
  )
  (list pts bulges has3d)
 )
)

(defun line-entity->segment (elst / p1 p2)
  (defun pt2d-from-dxf (vx vy)
    (cond
      ((and (listp vx) (>= (length vx) 2)) (list (car vx) (cadr vx)))
      ((and (numberp vx) (numberp vy)) (list vx vy))
      (T nil)
    )
  )
  (setq p1 (pt2d-from-dxf (dxf-get 10 elst) (dxf-get 20 elst))
        p2 (pt2d-from-dxf (dxf-get 11 elst) (dxf-get 21 elst)))
  (if (and p1 p2
           (numberp (car p1)) (numberp (cadr p1))
           (numberp (car p2)) (numberp (cadr p2)))
    (if (and (equal (car p1) (car p2) 1e-14) (equal (cadr p1) (cadr p2) 1e-14))
      nil
      (list p1 p2)
    )
    nil)
)

(defun remove-nth (i lst / k res)
  (setq k 0 res '())
  (while (< k (length lst))
    (if (/= k i) (setq res (append res (list (nth k lst)))))
    (setq k (1+ k))
  )
  res
)

(defun connect-lines-into-loops (segs / loops path closed progressed i n lastp headp s p1 p2)
  (setq loops '())
  (while segs
    (setq path (car segs))
    (setq segs (cdr segs))
    (if (and (listp path) (= (length path) 2) (listp (car path)) (listp (cadr path)))
      (progn
        (setq path (list (car path) (cadr path)))
        (setq closed nil)
        (while (not closed)
          (setq progressed nil)
          ;; extend at tail
          (setq lastp (last-elt path))
          (setq i 0 n (length segs))
          (while (and (< i n) (not progressed))
            (setq s (nth i segs))
            (if (and (listp s) (= (length s) 2))
              (progn
                (setq p1 (car s) p2 (cadr s))
                (cond
                  ((nearly-equal-pt2d lastp p1)
                   (setq path (append path (list p2)))
                   (setq segs (remove-nth i segs))
                   (setq progressed T)
                  )
                  ((nearly-equal-pt2d lastp p2)
                   (setq path (append path (list p1)))
                   (setq segs (remove-nth i segs))
                   (setq progressed T)
                  )
                  (T (setq i (1+ i)))
                )
              )
              (setq i (1+ i))
            )
          )
          ;; extend at head if still open
          (if (not progressed)
            (progn
              (setq headp (car path))
              (setq i 0 n (length segs))
              (while (and (< i n) (not progressed))
                (setq s (nth i segs))
                (if (and (listp s) (= (length s) 2))
                  (progn
                    (setq p1 (car s) p2 (cadr s))
                    (cond
                      ((nearly-equal-pt2d headp p1)
                       (setq path (cons p2 path))
                       (setq segs (remove-nth i segs))
                       (setq progressed T)
                      )
                      ((nearly-equal-pt2d headp p2)
                       (setq path (cons p1 path))
                       (setq segs (remove-nth i segs))
                       (setq progressed T)
                      )
                      (T (setq i (1+ i)))
                    )
                  )
                  (setq i (1+ i))
                )
              )
            )
          )
          ;; check closure
          (if (and (>= (length path) 4) (nearly-equal-pt2d (car path) (last-elt path)))
            (progn
              ;; drop duplicated last for nicer polygon list
              (setq loops (append loops (list (reverse (cdr (reverse path))))))
              (setq closed T)
            )
          )
          (if (and (not progressed) (not closed))
            (setq closed T)
          )
        )
      )
    )
  )
  loops
) 

(defun round-to-grid (x g)
  (* g (fix (/ x g)))
)

(defun canonicalize-segments (segs / res s p1 p2 g)
  (setq res '() g (max 1e-12 (areat-eps)))
  (foreach s segs
    (setq p1 (car s) p2 (cadr s))
    (if (and (listp p1) (listp p2)
             (numberp (car p1)) (numberp (cadr p1))
             (numberp (car p2)) (numberp (cadr p2)))
      (progn
        (setq p1 (list (round-to-grid (car p1) g)
                       (round-to-grid (cadr p1) g)))
        (setq p2 (list (round-to-grid (car p2) g)
                       (round-to-grid (cadr p2) g)))
        (if (not (and (equal (car p1) (car p2) 1e-14)
                       (equal (cadr p1) (cadr p2) 1e-14)))
          (setq res (cons (list p1 p2) res))
        )
      )
    )
  )
  (reverse res)
)

(defun is-lwpoly-closed (elst pts / flag)
  (setq flag (dxf-get 70 elst))
  (or (and flag (= (logand flag 1) 1))
      (and pts (nearly-equal-pt2d (car pts) (last-elt pts))))
)

;; --- Command -------------------------------------------------------------------

(defun c:areat (/ ss count i ent elst etype area total valid-count pts bulges hasBulge pinfo has3d lineSegs loops chordArea wedgeArea n nextIdx p1 p2 b skipReport)

  (setq total 0.0
        valid-count 0)

  (princ "\nSupported: CIRCLE, LWPOLYLINE (with arcs), POLYLINE (with arcs), LINE-closed loops.")
  (princ "\nPlease select objects...")

  (if (setq ss (ssget))
    (progn
      (setq count (sslength ss))
      (princ (strcat "\nNumber of selected objects: " (itoa count)))

      (setq i 0
            lineSegs '())
      (while (< i count)
        (setq ent  (ssname ss i))
        (setq elst (entget ent))
        (setq etype (dxf-get 0 elst))
        (setq area nil)
        (setq failReason nil)
        (setq skipReport nil)

        (cond
          ((= etype "CIRCLE")
           (setq area (* pi (expt (dxf-get 40 elst) 2.0)))
          )

          ((= etype "LWPOLYLINE")
           (setq pts (lwpoly-vertices-2d elst))
           (if (< (length pts) 3)
             (progn
               (princ (strcat "\n  [debug] LWPOLYLINE 10-cnt=" (itoa (length (dxf-filter-values 10 elst)))
                             ", 20-cnt=" (itoa (length (dxf-filter-values 20 elst)))
                             ", 10[0] type=" (if (and (dxf-filter-values 10 elst) (listp (car (dxf-filter-values 10 elst)))) "list" "num")
                             ", pts=" (itoa (length pts))))
               (setq failReason "LWPOLYLINE has fewer than 3 vertices")
             )
             (if (not (is-lwpoly-closed elst pts))
               (setq failReason "LWPOLYLINE is not closed")
               (progn
                 (setq area (lwpoly-area-from-elst elst))
                 (if (not area) (setq failReason "Failed to compute area (unexpected)"))
               )
             )
           )
          )

          ((= etype "POLYLINE")
           (setq pinfo (polyline-vertices-2d-with-bulges ent))
           (setq pts (car pinfo)
                 bulges (cadr pinfo)
                 has3d (caddr pinfo))
           (cond
             ((< (length pts) 3) (setq failReason "POLYLINE has fewer than 3 vertices"))
             ((/= (logand (dxf-get 70 (entget ent)) 1) 1) (setq failReason "POLYLINE is not closed (70 flag)"))
             (has3d (setq failReason "POLYLINE has non-zero Z (3D)"))
             (T
              (setq chordArea (polygon-area-shoelace pts))
              (setq wedgeArea 0.0
                    i 0
                    n (length pts))
              (while (< i n)
                (setq p1 (nth i pts)
                      nextIdx (if (< (1+ i) n) (1+ i) 0)
                      p2 (nth nextIdx pts)
                      b  (nth-or i bulges 0.0))
                (setq wedgeArea (+ wedgeArea (segment-arc-wedge-area p1 p2 b)))
                (setq i (1+ i))
              )
              (setq area (+ chordArea wedgeArea))
             )
           )
          )

          ((= etype "LINE")
           (setq pinfo (line-entity->segment elst))
           (if pinfo
             (setq lineSegs (append lineSegs (list pinfo)))
           )
           (setq skipReport T)
          )

          (t (setq area nil))
        )

        (if (and area (> area 0.0))
          (progn
            (setq total (+ total area)
                  valid-count (1+ valid-count))
            (princ (strcat "\n" (itoa (1+ i)) ". AREA: " (rtos area 2 6)))
          )
            (if (not skipReport)
              (progn
                (if failReason
                  (princ (strcat "\n" (itoa (1+ i)) ". Unable to calculate (" etype ") -> " failReason))
                  (princ (strcat "\n" (itoa (1+ i)) ". Unable to calculate (" etype ")"))
                )
              )
            )
        )

        (setq i (1+ i))
      )
 
      ;; If there were LINEs, attempt to connect into closed loops and add areas
      (if lineSegs
        (progn
          (setq lineSegs (canonicalize-segments lineSegs))
          (setq loops (connect-lines-into-loops lineSegs))
          (princ (strcat "\n[debug] LINE segments=" (itoa (length lineSegs)) ", loops=" (itoa (length loops)) ", eps=" (rtos (areat-eps) 2 6)))
          (setq i 0)
          (while (< i (length loops))
            (setq pts (nth i loops))
            (if (>= (length pts) 3)
              (progn
                (setq area (polygon-area-shoelace pts))
                (setq total (+ total area))
                (setq valid-count (1+ valid-count))
                (princ (strcat "\nLINE loop " (itoa (1+ i)) ". AREA: " (rtos area 2 6)))
              )
              (princ (strcat "\n[debug] loop " (itoa (1+ i)) " has insufficient vertices: " (itoa (length pts))))
            )
            (setq i (1+ i))
          )
        )
      )

      (princ "\n------------------------")
      (princ (strcat "\nTotal area: " (rtos total 2 6)))
      (princ (strcat "\nNumber of valid objects: " (itoa valid-count)))
      (princ "\n------------------------")
    )
    (princ "\nCancelled.") 
  )

  (princ) 
) 

;; Load message 
(princ "\nPure AutoLISP Area Calculator loaded!")
(princ "\nCommand: AREAT")
(princ) 