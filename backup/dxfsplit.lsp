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

(defun get-current-filename-without-ext ()
  ;; Get current drawing filename without extension
  (setq filename (getvar "DWGNAME"))
  (if filename
    (progn
      (setq dotpos (string-find-dot filename))
      (if dotpos
        (substr filename 1 dotpos)
        filename
      )
    )
    "dwg"
  )
)

;; 순수 AutoLISP로 문자열에서 점(.) 찾기
(defun string-find-dot (s / i n c pos)
  (setq i 1 n (strlen s) pos nil)
  (while (and (<= i n) (not pos))
    (setq c (substr s i 1))
    (if (= c ".") (setq pos i) (setq i (1+ i)))
  )
  pos
)

;; 한글 텍스트 처리를 위한 안전한 문자열 검증
(defun is-valid-text-content (text-content)
  (and text-content 
       (= (type text-content) 'STR) 
       (> (strlen text-content) 0)
  )
)

;; 텍스트 내용을 안전하게 출력
(defun safe-print-text (text-content)
  (if (is-valid-text-content text-content)
    text-content
    "[INVALID_TEXT]"
  )
)

(defun ensure-dwg-folder (/ dir)
  (setq dir (strcat (getvar "DWGPREFIX") (get-current-filename-without-ext) "\\"))
  ;; Create directory using Windows command (pure AutoLISP compatible)
  (command "_.SHELL" (strcat "if not exist \"" dir "\" mkdir \"" dir "\""))
  dir
)

(defun delete-existing-dwg-file (filepath)
  ;; Delete existing DWG file if it exists using Windows command
  (if (findfile filepath)
    (command "_.SHELL" (strcat "del \"" filepath "\""))
  )
)

(defun create-dwg-filepath (folder base-filename index / path)
  ;; Create filepath with index to ensure uniqueness
  (if (> (strlen base-filename) 0)
    (setq path (strcat folder base-filename "-" (itoa index) ".dwg"))
    (setq path (strcat folder "area-" (itoa index) ".dwg"))
  )
  path
)

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

(defun point-inside-bbox (pt bbox)
  ;; Check if point is inside bounding box
  (if (and pt bbox (listp pt) (listp bbox) (>= (length pt) 2) (>= (length bbox) 2))
    (and
      (numberp (car pt)) (numberp (car (car bbox))) (numberp (car (cadr bbox)))
      (numberp (cadr pt)) (numberp (cadr (car bbox))) (numberp (cadr (cadr bbox)))
      (>= (car pt) (car (car bbox)))
      (<= (car pt) (car (cadr bbox)))
      (>= (cadr pt) (cadr (car bbox)))
      (<= (cadr pt) (cadr (cadr bbox)))
    )
    nil
  )
)

(defun is-whitespace-char (c)
  (or (= c " ") (= c "\t") (= c "\n") (= c "\r"))
)

(defun trim-string (s / len start end)
  (if (not (and s (= (type s) 'STR)))
    ""
    (progn
      (setq len (strlen s)
            start 1
            end len)
      (while (and (<= start len) (is-whitespace-char (substr s start 1)))
        (setq start (1+ start)))
      (while (and (>= end start) (is-whitespace-char (substr s end 1)))
        (setq end (1- end)))
      (if (> end len)
        ""
        (if (> end 0)
          (if (> start end)
            ""
            (substr s start (1+ (- end start)))
          )
          ""
        )
      )
    )
  )
)

(defun replace-par-breaks (s / len i result char next-char)
  (if (not (and s (= (type s) 'STR)))
    ""
    (progn
      (setq len (strlen s)
            i 1
            result "")
      (while (<= i len)
        (setq char (substr s i 1))
        (if (= char "\\")
          (if (< i len)
            (progn
              (setq next-char (substr s (1+ i) 1))
              (cond
                ((or (= next-char "P") (= next-char "n"))
                 (setq result (strcat result " "))
                 (setq i (+ i 2))
                )
                (T
                 (setq result (strcat result char))
                 (setq i (1+ i))
                )
              )
            )
            (progn
              (setq result (strcat result char))
              (setq i (1+ i))
            )
          )
          (progn
            (setq result (strcat result char))
            (setq i (1+ i))
          )
        )
      )
      result
    )
  )
)

(defun normalize-text (s)
  (trim-string (replace-par-breaks s))
)

(defun merge-text-fragments (fragments / result frag)
  (setq result "")
  (foreach frag fragments
    (if (and frag (= (type frag) 'STR) (> (strlen frag) 0))
      (if (> (strlen result) 0)
        (setq result (strcat result " " frag))
        (setq result frag)
      )
    )
  )
  result
)

(defun collect-entity-text (elst / etype base extras fragments combined texts)
  (setq texts '()
        etype (dxf-get 0 elst))
  (cond
    ((or (= etype "TEXT") (= etype "ATTRIB") (= etype "ATTDEF"))
     (setq base (normalize-text (dxf-get 1 elst)))
     (if (> (strlen base) 0)
       (setq texts (list base))
     )
    )
    ((= etype "MTEXT")
     (setq base (normalize-text (dxf-get 1 elst)))
     (setq extras (dxf-filter-values 3 elst))
     (setq fragments '())
     (if (> (strlen base) 0)
       (setq fragments (append fragments (list base)))
     )
     (foreach extra extras
       (setq extra (normalize-text extra))
       (if (> (strlen extra) 0)
         (setq fragments (append fragments (list extra)))
       )
     )
     (setq combined (normalize-text (merge-text-fragments fragments)))
     (if (> (strlen combined) 0)
       (setq texts (list combined))
     )
    )
  )
  texts
)

(defun get-entity-anchor-point (elst / c10 c20 c11 c21)
  (setq c10 (dxf-get 10 elst)
        c20 (dxf-get 20 elst))
  (cond
    ((and (numberp c10) (numberp c20)) (list c10 c20))
    ((and (listp c10) (>= (length c10) 2)) (list (car c10) (cadr c10)))
    (T
     (setq c11 (dxf-get 11 elst)
           c21 (dxf-get 21 elst))
     (cond
       ((and (listp c11) (>= (length c11) 2)) (list (car c11) (cadr c11)))
       ((and (numberp c11) (numberp c21)) (list c11 c21))
       (T nil)
     )
    )
  )
)

(defun collect-insert-attribute-texts (insert / texts next elst etype)
  (setq texts '()
        next (entnext insert))
  (while next
    (setq elst (entget next)
          etype (dxf-get 0 elst))
    (cond
      ((= etype "ATTRIB")
       (setq texts (append texts (collect-entity-text elst)))
       (setq next (entnext next))
      )
      ((= etype "SEQEND") (setq next nil))
      (T (setq next nil))
    )
  )
  texts
)

(defun collect-block-definition-texts (block-name / texts blk ent elst etype)
  (setq texts '())
  (if (and block-name (= (type block-name) 'STR))
    (progn
      (setq blk (tblobjname "BLOCK" block-name))
      (if (not blk)
        (setq blk (cdr (assoc -1 (tblsearch "BLOCK" block-name))))
      )
      (if blk
        (progn
          (setq ent (entnext blk))
          (while (and ent (setq elst (entget ent)) (/= (dxf-get 0 elst) "ENDBLK"))
            (setq etype (dxf-get 0 elst))
            (if (or (= etype "TEXT") (= etype "MTEXT") (= etype "ATTDEF"))
              (setq texts (append texts (collect-entity-text elst)))
            )
            (setq ent (entnext ent))
          )
        )
      )
    )
  )
  texts
)

(defun dedupe-texts (texts / unique seen txt)
  (setq unique '()
        seen '())
  (foreach txt texts
    (if (and txt (= (type txt) 'STR) (> (strlen txt) 0))
      (if (not (member txt seen))
        (progn
          (setq unique (cons txt unique))
          (setq seen (cons txt seen))
        )
      )
    )
  )
  (reverse unique)
)

(defun get-texts-inside-bbox (bbox / texts text-ents e elst pt i)
  ;; Collect TEXT and MTEXT entities inside the bounding box
  (setq texts '()
        text-ents (ssget "X" '((0 . "TEXT,MTEXT"))))
  (if text-ents
    (progn
      (setq i 0)
      (while (< i (sslength text-ents))
        (setq e (ssname text-ents i)
              elst (entget e)
              pt (get-entity-anchor-point elst))
        (if (and pt (point-inside-bbox pt bbox))
          (setq texts (append texts (collect-entity-text elst)))
        )
        (setq i (1+ i))
      )
    )
  )
  (dedupe-texts texts)
)

(defun get-texts-from-block-inside-bbox (bbox / texts block-ents e elst block-name pt attr-texts def-texts i)
  ;; Find block references inside the bounding box and collect nested text
  (setq texts '()
        block-ents (ssget "X" '((0 . "INSERT"))))
  (if block-ents
    (progn
      (setq i 0)
      (while (< i (sslength block-ents))
        (setq e (ssname block-ents i)
              elst (entget e)
              block-name (dxf-get 2 elst)
              pt (get-entity-anchor-point elst))
        (if (and pt block-name (point-inside-bbox pt bbox))
          (progn
            (setq attr-texts (collect-insert-attribute-texts e))
            (setq def-texts (collect-block-definition-texts block-name))
            (setq texts (append texts attr-texts))
            (setq texts (append texts def-texts))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (dedupe-texts texts)
)

(defun sanitize-filename (text / result i char)
  ;; Remove or replace invalid filename characters
  (setq result ""
        i 1)
  (if (and text (> (strlen text) 0))
    (progn
      (while (<= i (strlen text))
        (setq char (substr text i 1))
        (if (or (= char "\\") (= char "/") (= char ":") (= char "*") 
                (= char "?") (= char "\"") (= char "<") (= char ">") 
                (= char "|") (= char " ") (= char "\t") (= char "\n"))
          (setq result (strcat result "_"))
          (setq result (strcat result char))
        )
        (setq i (1+ i))
      )
    )
    (setq result "invalid_text")
  )
  result
)

(defun create-filename-from-texts (texts / filename)
  ;; Create filename from text list
  (setq texts (dedupe-texts texts))
  (if texts
    (progn
      (setq filename "")
      (foreach text texts
        (if (> (strlen filename) 0)
          (setq filename (strcat filename "-" (sanitize-filename text)))
          (setq filename (sanitize-filename text))
        )
      )
      ;; Limit filename length
      (if (> (strlen filename) 50)
        (setq filename (substr filename 1 50))
      )
      filename
    )
    "area"
  )
) 

(defun export-selection-to-dwg (ss filepath / ok old-expert)
  (setq ok nil)
  (if (and ss (> (sslength ss) 0))
    (progn
      ;; Set EXPERT system variable to suppress prompts
      (setq old-expert (getvar "EXPERT"))
      (setvar "EXPERT" 2)
      ;; Try to use WBLOCK command with selection
      (command "_.WBLOCK" filepath "" '(0 0 0) ss "")
      ;; Restore EXPERT setting
      (setvar "EXPERT" old-expert)
      (if (findfile filepath) 
        (setq ok T)
      )
    )
  )
  ok) 

(defun c:DXFRECT (/ sel cnt i e folder filepath ss polyPts3d pts bbox texts base-filename block-texts)
  (princ "\nSelect blue polylines...")
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
            ;; Get bounding box of the polyline
            (setq bbox (bbox-of-pts pts))
            ;; Find texts inside the bounding box (including texts in blocks)
            (setq texts (get-texts-inside-bbox bbox))
            ;; Also get texts from blocks inside the bounding box
            (setq block-texts (get-texts-from-block-inside-bbox bbox))
            ;; Combine both text lists
            (setq texts (append texts block-texts))
            ;; Create base filename from texts
            (setq base-filename (create-filename-from-texts texts))
            ;; Generate filepath with index (overwrite mode)
            (setq filepath (create-dwg-filepath folder base-filename (1+ i)))
            ;; Delete existing file if it exists
            (delete-existing-dwg-file filepath)
            ;; Use Window-Polygon selection inside the rectangle
            (setq polyPts3d (pts-2d->3d (ensure-closed-pts pts)))
            (setq ss (ssget "WP" polyPts3d))
            ;; 항상 경계 폴리선을 포함하여 파일이 생성되도록 보장
            (if ss
              (setq ss (ssadd e ss))
              (progn (setq ss (ssadd)) (setq ss (ssadd e ss)))
            )
            (if (export-selection-to-dwg ss filepath)
              (princ (strcat "\nSaved: " filepath))
              (princ (strcat "\nSave failed: " filepath))
            )
          )
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo blue polylines selected.")
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
  