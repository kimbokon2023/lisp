;; ===============================================================================
;; Pure AutoLISP Area Calculator (DXF-based)
;; Command: AAAA ( Area)
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

;; Table options
(setq *areat-texth* 250.0) 

(defun areat-texth ()
  (if (and *areat-texth* (numberp *areat-texth*) (> *areat-texth* 0.0)) *areat-texth* 250.0)
)

;; Unit scale for displaying total area
(setq *areat-total-scale* 1e-6) ;; default: mm^2 to m^2

(defun areat-total-scale ()
  (if (and *areat-total-scale* (numberp *areat-total-scale*) (> *areat-total-scale* 0.0)) *areat-total-scale* 1.0)
)

(defun c:areatunit (/ s)
  (princ "\nTotal area display unit:")
  (princ "\n  1) no-scale (drawing units^2)") 
  (princ "\n  2) mm^2 -> m^2 (x1e-6)")
  (setq s (getint "\nSelect [1/2] <2>: "))
  (cond
    ((= s 1) (setq *areat-total-scale* 1.0))
    (T (setq *areat-total-scale* 1e-6))
  )
  (princ (strcat "\n[AAAA] total scale set to " (rtos (areat-total-scale) 2 6)))
  (princ)
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
  (princ (strcat "\n[AAAA] eps set to " (rtos (areat-eps) 2 6)))
  (princ)
)

(defun nth-or (i lst def)
  (if (and (numberp i) (>= i 0) (< i (length lst)))
    (nth i lst)
    def
  )
)

;; String util: return T if non-empty string
(defun non-empty-str (s)
  (and s (= (type s) 'STR) (> (strlen s) 0)))

(defun ensure-string (v)
  (cond
    ((and v (= (type v) 'STR)) v)
    ((numberp v) (rtos v 2 6))
    (T "")
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

;; --- Geometry utils -------------------------------------------------------------

(defun bbox-of-pts (pts / minx miny maxx maxy p)
  (if (not pts) nil
    (progn
      (setq minx (car (car pts))
            miny (cadr (car pts))
            maxx minx
            maxy miny)
      (foreach p (cdr pts)
        (if (< (car p) minx) (setq minx (car p)))
        (if (< (cadr p) miny) (setq miny (cadr p)))
        (if (> (car p) maxx) (setq maxx (car p)))
        (if (> (cadr p) maxy) (setq maxy (cadr p))))
      (list (list minx miny 0.0) (list maxx maxy 0.0))
    )
  )
)

(defun oddp (n) (= (rem n 2) 1))

(defun point-in-polygon (pt pts / cnt i n a b xi xj yi yj px py inter)
  ;; Ray casting algorithm
  (setq cnt 0 i 0 n (length pts)
        px (car pt) py (cadr pt))
  (while (< i n)
    (setq a (nth i pts)
          b (nth (if (< (1+ i) n) (1+ i) 0) pts))
    (setq yi (cadr a) yj (cadr b) xi (car a) xj (car b))
    (if (and (/= yi yj)
             (>= py (min yi yj))
             (<  py (max yi yj)))
      (progn
        (setq inter (+ xi (* (/ (- xj xi) (- yj yi)) (- py yi))))
        (if (> inter px) (setq cnt (1+ cnt)))
      )
    )
    (setq i (1+ i))
  )
  (oddp cnt)
)

;; --- Thickness extraction -------------------------------------------------------

(defun sanitize-number-string (s / i c out)
  (setq i 0 out "")
  (while (< i (strlen s))
    (setq c (substr s (1+ i) 1))
    (if (or (and (>= c "0") (<= c "9")) (= c "."))
      (setq out (strcat out c))
    )
    (setq i (1+ i))
  )
  out
)

;; Helpers to parse numbers adjacent to 'T'/'t'
(defun char-digit-or-dot-p (c)
  (or (and (>= c "0") (<= c "9")) (= c ".")))

(defun skip-spaces-and-seps-right (s i / n c)
  (setq n (strlen s))
  (while (and (<= i n)
              (member (substr s i 1) '(" " "\t" "=" ":")))
    (setq i (1+ i)))
  i)

(defun skip-spaces-and-seps-left (s i / c)
  (while (and (>= i 1)
              (member (substr s i 1) '(" " "\t" "=" ":")))
    (setq i (1- i)))
  i)

(defun extract-adjacent-number-around-T (txt / s n idx i startL lenL startR lenR numL numR)
  (setq s (strcase (if txt txt "")))
  (setq n (strlen s))
  (setq idx 1)
  (setq numL "" numR "")
  ;; find first 'T'
  (while (and (<= idx n) (/= (substr s idx 1) "T"))
    (setq idx (1+ idx)))
  (if (> idx n)
    ""
    (progn
      ;; left side
      (setq i (1- idx))
      (setq i (skip-spaces-and-seps-left s i))
      (setq startL i)
      (while (and (>= i 1) (char-digit-or-dot-p (substr s i 1)))
        (setq i (1- i)))
      (setq startL (1+ i))
      (setq lenL (- idx startL))
      (if (> lenL 0)
        (setq numL (substr s startL lenL))
      )
      ;; right side
      (setq i (1+ idx))
      (setq i (skip-spaces-and-seps-right s i))
      (setq startR i)
      (while (and (<= i n) (char-digit-or-dot-p (substr s i 1)))
        (setq i (1+ i)))
      (setq lenR (- i startR))
      (if (> lenR 0)
        (setq numR (substr s startR lenR))
      )
      ;; prefer number directly adjacent to T: left first, then right
      (cond
        ((and numL (> (strlen numL) 0)) (sanitize-number-string numL))
        ((and numR (> (strlen numR) 0)) (sanitize-number-string numR))
        (T ""))
    )
  )
)

(defun extract-thickness-from-text (txt / s adj)
  (if (not txt) 0.0
    (progn
      (setq adj (extract-adjacent-number-around-T txt))
      (if (> (strlen adj) 0)
        (atof adj)
        (progn
          ;; fallback: any numbers in text
          (setq s (sanitize-number-string (strcase txt)))
          (if (= s "") 0.0 (atof s))
        )
      )
    )
  )
)

;; Classifiers for TEXT contents
(defun is-thickness-text (txt)
  ;; T 주변에 숫자가 실제로 붙어있는지를 기준으로 판별
  (and txt
       (wcmatch (strcase txt) "*T*")
       (> (strlen (extract-adjacent-number-around-T txt)) 0)))

(defun is-workplace-text (txt)
  (and txt (not (is-thickness-text txt))))

(defun find-thickness-in-pts (pts / bb ss i ent elst ipt txt t)
  (setq t 0.0)
  (if (setq bb (bbox-of-pts pts))
    (progn
      (setq ss (ssget "C" (car bb) (cadr bb) '((0 . "TEXT,MTEXT"))))
      (if ss
        (progn
          (setq i 0)
          (while (and (< i (sslength ss)) (= t 0.0))
            (setq ent (ssname ss i)
                  elst (entget ent)
                  ipt  (cdr (assoc 10 elst))
                  txt  (cdr (assoc 1 elst)))
            (if (and ipt (listp ipt) (>= (length ipt) 2)
                     (point-in-polygon ipt pts)
                     (is-thickness-text txt))
              (setq t (extract-thickness-from-text txt))
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  t
)

(defun find-thickness-in-circle (center radius / bb ss i ent elst ipt txt t dx dy)
  (setq t 0.0)
  (setq bb (list (list (- (car center) radius) (- (cadr center) radius) 0.0)
                 (list (+ (car center) radius) (+ (cadr center) radius) 0.0)))
  (setq ss (ssget "C" (car bb) (cadr bb) '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (while (and (< i (sslength ss)) (= t 0.0))
        (setq ent (ssname ss i)
              elst (entget ent)
              ipt  (cdr (assoc 10 elst))
              txt  (cdr (assoc 1 elst))
              dx (- (car ipt) (car center))
              dy (- (cadr ipt) (cadr center)))
        (if (and (<= (+ (* dx dx) (* dy dy)) (* radius radius))
                 (is-thickness-text txt))
          (setq t (extract-thickness-from-text txt))
        )
        (setq i (1+ i))
      )
    )
  )
  t
)

;; Workplace name finders (TEXT without 'T')
(defun find-workplace-in-pts (pts / bb ss i ent elst ipt txt name)
  (setq name nil)
  (if (setq bb (bbox-of-pts pts))
    (progn
      (princ (strcat "\n[debug] searching for workplace text in bbox"))
      (setq ss (ssget "C" (car bb) (cadr bb) '((0 . "TEXT,MTEXT"))))
      (if ss
        (progn
          (princ (strcat "\n[debug] found " (itoa (sslength ss)) " text entities"))
          (setq i 0)
          (while (and (< i (sslength ss)) (not name))
            (setq ent (ssname ss i)
                  elst (entget ent)
                  ipt  (cdr (assoc 10 elst))
                  txt  (cdr (assoc 1 elst)))
            (princ "\n[debug] checking text: '")
            (if txt (princ txt) (princ "nil"))
            (princ "' at point")
            (if (and ipt (listp ipt) (>= (length ipt) 2)
                     (point-in-polygon ipt pts))
              (progn
                (princ (strcat "\n[debug] text is inside polygon"))
                (if (is-workplace-text txt)
                  (progn
                    (princ "\n[debug] identified as workplace text: '")
                    (princ txt)
                    (princ "'")
                    (setq name txt)
                  )
                  (progn
                    (princ "\n[debug] not workplace text (is thickness): '")
                    (if txt (princ txt) (princ "nil"))
                    (princ "'")
                  )
                )
              )
              (princ (strcat "\n[debug] text not inside polygon or invalid point"))
            )
            (setq i (1+ i))
          )
          ;; fallback: if none strictly inside polygon, pick first workplace TEXT in bbox
          (if (not name)
            (progn
              (setq i 0)
              (while (and (< i (sslength ss)) (not name))
                (setq ent (ssname ss i)
                      elst (entget ent)
                      txt  (cdr (assoc 1 elst)))
                (if (is-workplace-text txt)
                  (setq name txt)
                )
                (setq i (1+ i))
              )
            )
          )
        )
      )
    )
  )
  (princ "\n[debug] find-workplace-in-pts returning: '")
  (if name (princ name) (princ "nil"))
  (princ "'")
  name
)

(defun find-workplace-in-circle (center radius / bb ss i ent elst ipt txt name dx dy)
  (setq name nil)
  (setq bb (list (list (- (car center) radius) (- (cadr center) radius) 0.0)
                 (list (+ (car center) radius) (+ (cadr center) radius) 0.0)))
  (princ (strcat "\n[debug] searching for workplace text in circle, radius=" (rtos radius 2 2)))
  (setq ss (ssget "C" (car bb) (cadr bb) '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (princ (strcat "\n[debug] found " (itoa (sslength ss)) " text entities in circle"))
      (setq i 0)
      (while (and (< i (sslength ss)) (not name))
        (setq ent (ssname ss i)
              elst (entget ent)
              ipt  (cdr (assoc 10 elst))
              txt  (cdr (assoc 1 elst))
              dx (- (car ipt) (car center))
              dy (- (cadr ipt) (cadr center)))
        (princ "\n[debug] checking circle text: '")
        (if txt (princ txt) (princ "nil"))
        (princ "' distance=")
        (princ (rtos (sqrt (+ (* dx dx) (* dy dy))) 2 2))
        (if (<= (+ (* dx dx) (* dy dy)) (* radius radius))
          (progn
            (princ (strcat "\n[debug] text is inside circle"))
            (if (is-workplace-text txt)
              (progn
                (princ "\n[debug] identified as workplace text: '")
                (princ txt)
                (princ "'")
                (setq name txt)
              )
              (progn
                (princ "\n[debug] not workplace text (is thickness): '")
                (if txt (princ txt) (princ "nil"))
                (princ "'")
              )
            )
          )
          (princ (strcat "\n[debug] text outside circle"))
        )
        (setq i (1+ i))
      )
      ;; fallback: if none strictly inside circle, pick first workplace TEXT in bbox
      (if (not name)
        (progn
          (setq i 0)
          (while (and (< i (sslength ss)) (not name))
            (setq ent (ssname ss i)
                  elst (entget ent)
                  txt  (cdr (assoc 1 elst)))
            (if (is-workplace-text txt)
              (setq name txt)
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  (princ "\n[debug] find-workplace-in-circle returning: '")
  (if name (princ name) (princ "nil"))
  (princ "'")
  name
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

;; --- Table rendering ------------------------------------------------------------

(defun areat-entmake-line (p1 p2)
  (command "_.LINE"
           (list (car p1) (cadr p1) 0.0)
           (list (car p2) (cadr p2) 0.0)
           "")
)

(defun areat-entmake-text (pt h str)
  (setq str (ensure-string str))
  (if (non-empty-str str)
    (command "_.TEXT"
             (list (car pt) (cadr pt) 0.0)
             (max 1.0 (abs h))
             0.0
             str)
  )
)

(defun areat-entmake-text-just (alpt h str halign valign)
  ;; Basic TEXT with optional right justification when halign=2 (right)
  (setq str (ensure-string str))
  (if (non-empty-str str)
    (if (= halign 2)
      (command "_.TEXT" "J" "R"
               (list (car alpt) (cadr alpt) 0.0)
               (max 1.0 (abs h))
               0.0
               str)
      (command "_.TEXT"
               (list (car alpt) (cadr alpt) 0.0)
               (max 1.0 (abs h))
               0.0
               str)
    )
  )
)

(defun areat-format-thk (tval / s)
  ;; Ensure leading zero for decimals like .8 -> 0.80 and fix to 2 decimals
  (cond
    ((not (numberp tval)) "0.00")
    ((< tval 0.0) (strcat "-" (areat-format-thk (- tval))))
    (T
     (setq s (rtos tval 2 2))
     ;; Some locales may return ".80" when <1; guard it
     (if (= (substr s 1 1) ".")
       (setq s (strcat "0" s))
     )
     s))
)

;; --- Number formatting (no VL functions) ----------------------------------------
(defun areat-string-find-dot (s / i n c pos)
  (setq i 1 n (strlen s) pos nil)
  (while (and (<= i n) (not pos))
    (setq c (substr s i 1))
    (if (= c ".") (setq pos i) (setq i (1+ i)))
  )
  pos)

(defun areat-insert-commas (intstr / len i res rem)
  (setq len (strlen intstr)
        i len
        res ""
        rem 0)
  (while (> i 0)
    (setq res (strcat (substr intstr i 1) res))
    (setq rem (1+ rem))
    (if (and (> i 1) (= rem 3))
      (progn (setq res (strcat "," res)) (setq rem 0))
    )
    (setq i (1- i))
  )
  res)

(defun areat-format-number (val decimals / s sign absval dotpos intpart fracpart)
  (setq sign (if (< val 0.0) "-" ""))
  (setq absval (abs val))
  (setq s (rtos absval 2 (max 0 decimals)))
  (setq dotpos (areat-string-find-dot s))
  (if dotpos
    (progn
      (setq intpart (substr s 1 (1- dotpos)))
      (setq fracpart (substr s (+ dotpos 1)))
      (strcat sign (areat-insert-commas intpart)
              (if (> (strlen fracpart) 0) (strcat "." fracpart) ""))
    )
    (strcat sign (areat-insert-commas s))
  )
)

(defun areat-draw-table (ins rows totalVol totalArea / baseH textH rowH w1 w2 w3 w4 w5 x0 y0 x1 x2 x3 x4 x5 m r y tpad xpad row no work areaStr thkStr volStr remStr totalAreaStr totalVolStr ytxt dispNo areaOut volOut)
  ;; Use larger, more readable text for professional appearance
  (setq baseH (areat-texth)
        textH (* baseH 0.3))    ; Increased from 0.2 to 0.3 for better readability
  (setq rowH (* textH 2.2))     ; Increased row height for better spacing
  ;; Adjusted column widths: 2열부터 마지막 열까지 너비 700 축소
  (setq w1 (* textH 8.0)       ; No - keep original width
        w2 (* textH 23.0)      ; Workplace - reduced by 700 (30.0 -> 23.0)
        w3 (* textH 15.0)      ; Area - reduced by 700 (22.0 -> 15.0)
        w4 (* textH 15.0)      ; Thickness - reduced by 700 (22.0 -> 15.0)
        w5 (* textH 18.0))     ; Volume - reduced by 700 (25.0 -> 18.0)
  (setq x0 (car ins)
        y0 (cadr ins)    ; 삽입점과 테이블 그리드를 일치시켜 테두리-텍스트 어긋남 방지
        x1 (+ x0 w1)
        x2 (+ x1 w2)
        x3 (+ x2 w3)
        x4 (+ x3 w4)
        x5 (+ x4 w5))
  (setq tpad (* textH 0.8)        ; vertical padding
        xpad (* textH 1.4))       ; horizontal padding 살짝 증가
  ;; number of table rows: header + data rows + total
  (setq m (+ 2 (length rows)))
  ;; outer box and grid
  (setq r 0)
  (while (<= r m)
    (setq y (- y0 (* r rowH)))
    (areat-entmake-line (list x0 y) (list x5 y))
    (setq r (1+ r))
  )
  (areat-entmake-line (list x0 y0) (list x0 (- y0 (* m rowH))))
  (areat-entmake-line (list x1 y0) (list x1 (- y0 (* m rowH))))
  (areat-entmake-line (list x2 y0) (list x2 (- y0 (* m rowH))))
  (areat-entmake-line (list x3 y0) (list x3 (- y0 (* m rowH))))
  (areat-entmake-line (list x4 y0) (list x4 (- y0 (* m rowH))))
  (areat-entmake-line (list x5 y0) (list x5 (- y0 (* m rowH))))
  ;; header
  (setq y (- y0 (* 0 rowH)))
  ;; Header text: 베이스라인을 셀 중앙에 가깝게 배치
  (setq ytxt (- y (* rowH 0.60)))
  (areat-entmake-text-just (list (- x1 xpad) ytxt) textH "No" 2 2)
  (areat-entmake-text-just (list (+ x1 xpad) ytxt) textH "workplace" 0 2)
  (areat-entmake-text-just (list (- x3 xpad) ytxt) textH "Area (mm^2)" 2 2)
  (areat-entmake-text-just (list (- x4 xpad) ytxt) textH "Thickness (mm)" 2 2)
  (areat-entmake-text-just (list (- x5 xpad) ytxt) textH "Volume (mm^3)" 2 2)  
  ;; data rows
  (setq r 0)
  (while (< r (length rows))
    (setq row (nth r rows)
          no (nth 0 row)
          work (nth 1 row)
          areaStr (nth 2 row)
          thkStr  (nth 3 row) 
          volStr  (nth 4 row) 
          remStr  (nth 5 row)
          y (- y0 (* (+ 1 r) rowH)))
    (setq ytxt (- y (* rowH 0.60)))
    ;; numbering and number formatting
    (setq dispNo (itoa (1+ r)))
    (setq areaOut (areat-format-number (if areaStr (atof areaStr) 0.0) 0))
    (setq volOut  (areat-format-number (if volStr (atof volStr) 0.0) 0))
    ;; Text positioned to sit visually centered within each cell
    (areat-entmake-text-just (list (- x1 xpad) ytxt) textH dispNo 2 2)
    (areat-entmake-text-just (list (+ x1 xpad) ytxt) textH work 0 2)
    (areat-entmake-text-just (list (- x3 xpad) ytxt) textH areaOut 2 2)
    (areat-entmake-text-just (list (- x4 xpad) ytxt) textH thkStr 2 2)
    (areat-entmake-text-just (list (- x5 xpad) ytxt) textH volOut 2 2)
    (setq r (1+ r))
  )
  ;; total row
  (setq y (- y0 (* (+ 1 (length rows)) rowH)))
  (setq totalAreaStr (areat-format-number totalArea 0))
  (setq totalVolStr  (areat-format-number totalVol 0))
  ;; Total row - 'Total' in workplace, numbers right aligned; thickness blank
  (setq ytxt (- y (* rowH 0.60)))
  (areat-entmake-text-just (list (- x1 xpad) ytxt) textH "" 2 2)
  (areat-entmake-text-just (list (+ x1 xpad) ytxt) textH "Total" 0 2)
  (areat-entmake-text-just (list (- x3 xpad) ytxt) textH totalAreaStr 2 2)
  (areat-entmake-text-just (list (- x4 xpad) ytxt) textH "" 2 2)
  (areat-entmake-text-just (list (- x5 xpad) ytxt) textH totalVolStr 2 2)
  (princ "\n table complete.")
)

;; --- Command -------------------------------------------------------------------

(defun c:areat (/ ss count i ent elst etype area totalArea totalVol valid-count pts bulges hasBulge pinfo has3d lineSegs loops chordArea wedgeArea n nextIdx p1 p2 b skipReport tableRows workplace thk vol)

  (setq totalArea 0.0
        totalVol 0.0
        valid-count 0)

  (princ "\nSupported: CIRCLE, LWPOLYLINE (with arcs), POLYLINE (with arcs), LINE-closed loops.")
  (princ "\nPlease select objects...")

  ;; Exclude TEXT/MTEXT from selection; they are probed separately for thickness
  (if (setq ss (ssget '((0 . "CIRCLE,LWPOLYLINE,POLYLINE,LINE"))))
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
        (setq thk 0.0 vol 0.0)
        (setq workplace "")

        (cond
          ((= etype "CIRCLE")
           (setq area (* pi (expt (dxf-get 40 elst) 2.0)))
           (setq thk (find-thickness-in-circle (dxf-get 10 elst) (dxf-get 40 elst)))
           (setq workplace (find-workplace-in-circle (dxf-get 10 elst) (dxf-get 40 elst)))
           (if (not workplace) (setq workplace ""))
           ;; volume in mm^3: area(mm^2) * thickness(mm)
           (setq vol (* area thk))
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
                 (setq thk (find-thickness-in-pts pts))
                 (setq workplace (find-workplace-in-pts pts))
                 (if (not workplace) (setq workplace ""))
                 (setq vol (* area thk))
                 (if (not area) (setq failReason "Failed to compute area (unexpected)"))
               )
             )
           )
          )

          ((= etype "POLYLINE")
           (princ "\n[debug] processing POLYLINE")
           (setq pinfo (polyline-vertices-2d-with-bulges ent))
           (princ "\n[debug] got POLYLINE info")
           (setq pts (car pinfo)
                 bulges (cadr pinfo)
                 has3d (caddr pinfo))
           (princ (strcat "\n[debug] POLYLINE pts count: " (itoa (length pts))))
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
              (setq thk (find-thickness-in-pts pts))
              (setq workplace (find-workplace-in-pts pts))
              (if (not workplace) (setq workplace ""))
              (setq vol (* area thk))
             )
           )
          )

          ((= etype "LINE")
           (princ "\n[debug] processing LINE")
           (setq pinfo (line-entity->segment elst))
           (if pinfo
             (progn
               (princ "\n[debug] LINE segment created")
               (setq lineSegs (append lineSegs (list pinfo)))
             )
             (princ "\n[debug] LINE segment failed")
           )
           (setq skipReport T)
          )

          (t (setq area nil))
        )

        (if (and area (> area 0.0))
          (progn
            (setq totalArea (+ totalArea area)
                  totalVol  (+ totalVol (if vol vol 0.0))
                  valid-count (1+ valid-count))
            (setq typStr (cond
                           ((= etype "CIRCLE") "CIRCLE")
                           ((= etype "LWPOLYLINE") "LWPOLYLINE")
                           ((= etype "POLYLINE") "POLYLINE")
                           (T etype)))
            ;; Skip raw LINE rows; they will be represented later as LINE-LOOP
            (if (/= etype "LINE")
              (progn
                (princ "\n[debug] adding to table: No=")
                (princ (itoa (1+ i)))
                (princ ", workplace='")
                (if workplace (princ workplace) (princ "EMPTY"))
                (princ "', area=")
                (princ (rtos area 2 0))
          (setq tableRows (append tableRows (list (list (itoa (1+ i)) workplace (rtos area 2 0) (areat-format-thk thk) (rtos vol 2 0)))))
              )
            )
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
                (setq thk (find-thickness-in-pts pts))
                (setq workplace (find-workplace-in-pts pts))
                (if (not workplace) (setq workplace ""))
                (setq vol (* area thk))
                (setq totalArea (+ totalArea area))
                (setq totalVol (+ totalVol vol))
                (setq valid-count (1+ valid-count))
                (princ "\n[debug] adding LINE loop to table: No=")
                (princ (itoa (+ count (1+ i))))
                (princ ", workplace='")
                (if workplace (princ workplace) (princ "EMPTY"))
                (princ "', area=")
                (princ (rtos area 2 0))
                (setq tableRows (append tableRows (list (list (itoa (+ count (1+ i))) workplace (rtos area 2 0) (areat-format-thk thk) (rtos vol 2 0)))))
              )
              (princ (strcat "\n[debug] loop " (itoa (1+ i)) " has insufficient vertices: " (itoa (length pts))))
            )
            (setq i (1+ i))
          )
        )
      )

      (princ "\n------------------------")
      (princ (strcat "\nTotal area: " (rtos totalArea 2 6)))
      (princ (strcat "\nNumber of valid objects: " (itoa valid-count)))
      (princ "\n------------------------")
      (if (> valid-count 0)  
        (progn
          (princ "\nSpecify table insertion point...")
          (if (setq ins (getpoint "\nTable insertion point: "))
            (progn
              (princ "\n[debug] starting table creation")
              (areat-draw-table ins tableRows totalVol totalArea)
            )
          )
        )
      )
    )
    (princ "\nCancelled.")  
  )  
 
  (princ)  
)   

;; Command aliases
(defun c:AAAA () (c:areat))

;; Load message 
(princ "\nPure AutoLISP Area Calculator loaded!")
(princ "\nCommand: AAAA")
(princ)        