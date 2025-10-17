;; ===============================================================================
;; Command: DDDD (DWG Split)
;; ===============================================================================
;; This LISP routine automatically finds blue closed polylines (rectangles) in the
;; current drawing and exports each rectangle along with all objects inside it to
;; separate DWG files. Files are saved as dwg-1.dwg, dwg-2.dwg, etc. in a 'dwg'
;; subfolder within the current drawing's directory. Each file contains the
;; boundary rectangle plus all entities within its bounds.

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

(setq *dxfsplit-eps* 1e-6)    

(defun dxfsplit-eps ()
  (if (and *dxfsplit-eps* (numberp *dxfsplit-eps*)) *dxfsplit-eps* 1e-6)
)


(defun nearly-equal-pt2d (p1 p2 / eps)
  (setq eps (dxfsplit-eps))
  (and (<= (abs (- (car p1) (car p2))) eps)
       (<= (abs (- (cadr p1) (cadr p2))) eps))
)

;; (removed interactive eps setter)

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

;; (removed point-in-polygon as not needed)

;; (removed thickness/workplace extraction)

;; (removed workplace finders)

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

;; (removed area calculation helpers)

;; (removed bulge traversal for area)

;; (removed LINE loop helpers)

(defun is-lwpoly-closed (elst pts / flag)
  (setq flag (dxf-get 70 elst))
  (or (and flag (= (logand flag 1) 1))
      (and pts (nearly-equal-pt2d (car pts) (last-elt pts))))
)


;; (removed old area command and alias)

;; --- Rectangle-based DWG export -------------------------------------------------

(defun ensure-dwg-folder (/ dir)
  (setq dir (strcat (getvar "DWGPREFIX") "dwg\\"))
  ;; Create folder if missing (Windows)
  (startapp "cmd.exe" (strcat "/c if not exist \"" dir "\" mkdir \"" dir "\""))
  dir
)

(defun next-dwg-filepath (folder / i path)
  (setq i 1)
  (while (progn
           (setq path (strcat folder "dwg-" (itoa i) ".dwg"))
           (findfile path))
    (setq i (1+ i)))
  path)

(defun get-poly-pts-2d (e / elst etype)
  (setq elst (entget e)
        etype (dxf-get 0 elst))
  (cond
    ((= etype "LWPOLYLINE") (lwpoly-vertices-2d elst))
    ((= etype "POLYLINE") (car (polyline-vertices-2d e)))
    (T '())
  )
)

(defun is-poly-closed (e / elst etype)
  (setq elst (entget e)
        etype (dxf-get 0 elst))
  (cond
    ((= etype "LWPOLYLINE") (is-lwpoly-closed elst (lwpoly-vertices-2d elst)))
    ((= etype "POLYLINE") (= (logand (dxf-get 70 elst) 1) 1))
    (T nil)
  )
)

(defun pts-2d->3d (pts / res p)
  (setq res '())
  (foreach p pts
    (setq res (append res (list (list (car p) (cadr p) 0.0)))))
  res)

(defun bbox-width (pts / bb)
  (if (setq bb (bbox-of-pts pts))
    (- (car (cadr bb)) (car (car bb)))
    0.0))

(defun export-selection-to-dwg (ss filepath / ok)
  (setq ok nil)
  (if (and ss (> (sslength ss) 0))
    (progn
      ;; Try to use WBLOCK command with selection
      (command "_.WBLOCK" filepath "" '(0 0 0) ss "")
      (if (findfile filepath) 
        (setq ok T)
        ;; Fallback: try EXPORT command
             (progn
          (command "_.EXPORT" filepath ss "")
          (if (findfile filepath) (setq ok T))
        )
      )
    )
  )
  ok)

(defun c:DXFRECT (/ sel cnt i e folder filepath ss polyPts3d)
  (princ "\n파란색 사각형(닫힌 폴리선)을 선택하세요. 각각을 개별 DWG 파일로 저장합니다.")
  ;; Select blue closed polylines
  (if (setq sel (ssget '((0 . "LWPOLYLINE,POLYLINE") (62 . 5))))
             (progn
      (setq folder (ensure-dwg-folder))
      (setq cnt (sslength sel)
            i 0)
      (while (< i cnt)
        (setq e (ssname sel i))
        (setq pts (get-poly-pts-2d e))
        (if (and (>= (length pts) 3) (is-poly-closed e))
          (progn
            (setq filepath (next-dwg-filepath folder))
            ;; Use Window-Polygon selection inside the rectangle
            (setq polyPts3d (pts-2d->3d (ensure-closed-pts pts)))
            (setq ss (ssget "WP" polyPts3d))
            ;; 항상 경계 폴리선을 포함하여 파일이 생성되도록 보장
            (if ss
              (setq ss (ssadd e ss))
              (progn (setq ss (ssadd)) (setq ss (ssadd e ss)))
            )
            (if (export-selection-to-dwg ss filepath)
              (princ (strcat "\n저장됨: " filepath))
              (princ (strcat "\n저장 실패: " filepath))
            )
          )
        )
        (setq i (1+ i))
      )
    )
    (princ "\n파란색 사각형이 선택되지 않았습니다.")
  )
  (princ)  
)   

;; Alias for DWG export
(defun c:DDDD () (c:DXFRECT))
(defun c:DDDDD () (c:DXFRECT))

;; Load message 
(princ "\nPure AutoLISP DWG Split loaded!")
(princ "\nCommands: DDDD, DXFRECT")
(princ)        