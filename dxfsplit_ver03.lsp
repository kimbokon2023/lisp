;; ===============================================================================
;; Command: DDDD (DWG Split)
;; ===============================================================================
;; This LISP routine automatically finds closed polylines (rectangles) in the
;; current drawing—regardless of color—and exports each rectangle along with all objects inside it to
;; separate DWG files. Each file is named after the first text discovered inside the boundary, with
;; a numeric suffix to keep names unique. If no text is found, the file name defaults to "polyline".

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

(defun ensure-dwg-folder (/ dir cmd)
  (setq dir (strcat (getvar "DWGPREFIX") (get-current-filename-without-ext) "\\"))
  (setq cmd (strcat "chcp 65001>nul & if not exist \"" dir "\" mkdir \"" dir "\""))
  (command "_.SHELL" cmd)
  dir
)

(defun delete-existing-dwg-file (filepath / cmd)
  ;; Delete existing DWG file if it exists
  (if (findfile filepath)
    (progn
      (setq cmd (strcat "chcp 65001>nul & del \"" filepath "\""))
      (command "_.SHELL" cmd)
    )
  )
)

(defun create-dwg-filepath (folder base-filename / path)
  ;; Create filepath for temporary WBLOCK export
  (if (> (strlen base-filename) 0)
    (setq path (strcat folder base-filename ".dwg"))
    (setq path (strcat folder "polyline.dwg"))
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

;; 경계 상자의 중심점 계산
(defun bbox-center (bbox / minx miny maxx maxy)
  (if (and bbox (>= (length bbox) 2))
    (progn
      (setq minx (car (car bbox))
            miny (cadr (car bbox))
            maxx (car (cadr bbox))
            maxy (cadr (cadr bbox)))
      (list (/ (+ minx maxx) 2.0) (/ (+ miny maxy) 2.0) 0.0)
    )
    '(0.0 0.0 0.0)
  )
)

;; 선택 세트의 모든 객체를 기준점 중심으로 90도 회전
(defun rotate-selection-90 (ss center / i e old-cmdecho)
  (if (and ss (> (sslength ss) 0) center)
    (progn
      ;; 명령 에코 끄기
      (setq old-cmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)

      ;; ROTATE 명령으로 90도 회전
      (command "_.ROTATE" ss "" center "90")

      ;; 명령 에코 복원
      (setvar "CMDECHO" old-cmdecho)
      T
    )
    nil
  )
)

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

;; ============================================================================
;; 순수 AutoLISP 한글 처리: Unicode escape 수동 파싱
;; ============================================================================

;; 16진수 문자를 10진수로 변환
(defun hex-char-to-int (c / a)
  (setq a (ascii c))
  (cond
    ((and (>= a 48) (<= a 57)) (- a 48))   ; 0-9
    ((and (>= a 65) (<= a 70)) (- a 55))   ; A-F
    ((and (>= a 97) (<= a 102)) (- a 87))  ; a-f
    (T 0)
  )
)

;; 16진수 문자열을 10진수로 변환
(defun hex-to-int (hex-str / result i len)
  (setq result 0
        i 1
        len (strlen hex-str))
  (while (<= i len)
    (setq result (+ (* result 16) (hex-char-to-int (substr hex-str i 1))))
    (setq i (1+ i))
  )
  result
)

;; Unicode 코드포인트를 UTF-8 바이트 시퀀스로 변환
;; CADian의 chr 함수는 0-255만 지원하므로 UTF-8 인코딩 필요
(defun unicode-to-utf8-string (code / b1 b2 b3)
  (cond
    ;; 1바이트: 0x0000 - 0x007F (0-127)
    ((<= code 127)
     (chr code)
    )
    ;; 2바이트: 0x0080 - 0x07FF (128-2047)
    ((<= code 2047)
     (setq b1 (+ 192 (/ code 64)))           ; 110xxxxx
     (setq b2 (+ 128 (rem code 64)))         ; 10xxxxxx
     (strcat (chr b1) (chr b2))
    )
    ;; 3바이트: 0x0800 - 0xFFFF (2048-65535) - 한글 포함
    ((<= code 65535)
     (setq b1 (+ 224 (/ code 4096)))                    ; 1110xxxx
     (setq b2 (+ 128 (rem (/ code 64) 64)))             ; 10xxxxxx
     (setq b3 (+ 128 (rem code 64)))                    ; 10xxxxxx
     (strcat (chr b1) (chr b2) (chr b3))
    )
    ;; 4바이트는 한글에 필요없으므로 생략
    (T "")
  )
)

;; Unicode escape 시퀀스를 실제 문자로 변환
;; 지원 패턴: \U+XXXX, \\U+XXXX, \M+nXXXX (MIF 인코딩)
(defun decode-unicode-escapes (s / result i len char next-char next2 hex-code unicode-val utf8-str)
  (if (not (and s (= (type s) 'STR)))
    ""
    (progn
      (setq result ""
            i 1
            len (strlen s))
      (while (<= i len)
        (setq char (substr s i 1))
        (cond
          ;; 백슬래시로 시작하는 이스케이프 시퀀스 처리
          ((= char "\\")
           (if (< i len)
             (progn
               (setq next-char (substr s (1+ i) 1))
               (cond
                 ;; \U+ 패턴: Unicode escape
                 ((and (= next-char "U")
                       (<= (+ i 6) len)
                       (= (substr s (+ i 2) 1) "+"))
                  (setq hex-code (substr s (+ i 3) 4))
                  (setq unicode-val (hex-to-int hex-code))
                  (append-debug (strcat "Unicode 변환: \\U+" hex-code " -> " (itoa unicode-val)))
                  (if (and (> unicode-val 0) (< unicode-val 65536))
                    (progn
                      ;; Unicode 코드포인트를 UTF-8 바이트로 변환
                      (setq utf8-str (unicode-to-utf8-string unicode-val))
                      (append-debug (strcat "UTF-8 변환 결과: " (if utf8-str utf8-str "[FAILED]")))
                      (setq result (strcat result utf8-str))
                    )
                    (setq result (strcat result char next-char))
                  )
                  (setq i (+ i 7))
                 )
                 ;; \M+ 패턴: MIF 인코딩 (일부 구버전 CAD)
                 ((and (= next-char "M")
                       (<= (+ i 7) len)
                       (= (substr s (+ i 2) 1) "+"))
                  (setq next2 (substr s (+ i 3) 1))
                  (setq hex-code (substr s (+ i 4) 4))
                  (setq unicode-val (hex-to-int hex-code))
                  (if (and (> unicode-val 0) (< unicode-val 65536))
                    (progn
                      (setq utf8-str (unicode-to-utf8-string unicode-val))
                      (setq result (strcat result utf8-str))
                    )
                    (setq result (strcat result char next-char))
                  )
                  (setq i (+ i 8))
                 )
                 ;; 그 외 백슬래시 문자는 그대로 유지
                 (T
                  (setq result (strcat result char))
                  (setq i (1+ i))
                 )
               )
             )
             ;; 마지막 문자가 백슬래시인 경우
             (progn
               (setq result (strcat result char))
               (setq i (1+ i))
             )
           )
          )
          ;; 일반 문자는 그대로 추가
          (T
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
  (trim-string (decode-unicode-escapes (replace-par-breaks s)))
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

(defun collect-entity-text (elst / etype raw base extras raw-fragments raw-combined normalized texts dxf1-raw dxf3-raw)
  (setq texts '()
        etype (dxf-get 0 elst))
  (cond
    ((or (= etype "TEXT") (= etype "ATTRIB") (= etype "ATTDEF"))
     ;; DXF 그룹 코드 1의 원본 값 디버깅
     (setq dxf1-raw (dxf-get 1 elst))
     (append-debug (strcat "=== " etype " 엔티티 텍스트 추출 ==="))
     (append-debug (strcat "DXF(1) 원본: " (if dxf1-raw (ensure-string dxf1-raw) "[NIL]")))

     (setq raw (replace-par-breaks (ensure-string dxf1-raw)))
     (append-debug (strcat "Par-breaks 처리 후: " raw))

     (setq normalized (normalize-text raw))
     (append-debug (strcat "Normalize 후: " normalized))

     (if (> (strlen normalized) 0)
       (setq texts (list (cons normalized raw)))
     )
    )
    ((= etype "MTEXT")
     (append-debug "=== MTEXT 엔티티 텍스트 추출 ===")
     (setq raw-fragments '())

     (setq dxf1-raw (dxf-get 1 elst))
     (append-debug (strcat "DXF(1) 원본: " (if dxf1-raw (ensure-string dxf1-raw) "[NIL]")))

     (setq base (replace-par-breaks (ensure-string dxf1-raw)))
     (if (> (strlen (trim-string base)) 0)
       (setq raw-fragments (append raw-fragments (list base)))
     )

     (setq extras (dxf-filter-values 3 elst))
     (if extras
       (progn
         (append-debug (strcat "DXF(3) 개수: " (itoa (length extras))))
         (foreach extra extras
           (setq dxf3-raw extra)
           (append-debug (strcat "DXF(3) 원본: " (if dxf3-raw (ensure-string dxf3-raw) "[NIL]")))
           (setq extra (replace-par-breaks (ensure-string extra)))
           (if (> (strlen (trim-string extra)) 0)
             (setq raw-fragments (append raw-fragments (list extra)))
           )
         )
       )
     )

     (setq raw-combined (merge-text-fragments raw-fragments))
     (append-debug (strcat "병합된 원본: " raw-combined))

     (setq normalized (normalize-text raw-combined))
     (append-debug (strcat "Normalize 후: " normalized))

     (if (> (strlen normalized) 0)
       (setq texts (list (cons normalized raw-combined)))
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

(defun collect-insert-attribute-texts (insert bbox / texts next elst etype pt)
  (setq texts '()
        next (entnext insert))
  (while next
    (setq elst (entget next)
          etype (dxf-get 0 elst))
    (cond
      ((= etype "ATTRIB")
       (setq pt (get-entity-anchor-point elst))
       (if (or (not bbox) (point-inside-bbox pt bbox))
         (setq texts (append texts (collect-entity-text elst)))
       )
       (setq next (entnext next))
      )
      ((= etype "SEQEND") (setq next nil))
      (T (setq next nil))
    )
  )
  texts
)

(defun collect-block-definition-texts (block-name / texts blk ent elst etype text-count)
  (setq texts '()
        text-count 0)
  (if (and block-name (= (type block-name) 'STR))
    (progn
      (append-debug (strcat "=== 블록 정의 텍스트 추출: " block-name " ==="))
      (setq blk (tblobjname "BLOCK" block-name))
      (if (not blk)
        (setq blk (cdr (assoc -1 (tblsearch "BLOCK" block-name))))
      )
      (if blk
        (progn
          (append-debug "블록 정의 찾음")
          (setq ent (entnext blk))
          (while (and ent (setq elst (entget ent)) (/= (dxf-get 0 elst) "ENDBLK"))
            (setq etype (dxf-get 0 elst))
            (if (or (= etype "TEXT") (= etype "MTEXT") (= etype "ATTDEF"))
              (progn
                (setq text-count (1+ text-count))
                (append-debug (strcat "블록 내 " etype " 엔티티 #" (itoa text-count)))
                (setq texts (append texts (collect-entity-text elst)))
              )
            )
            (setq ent (entnext ent))
          )
          (append-debug (strcat "블록 내 총 텍스트 엔티티: " (itoa text-count)))
        )
        (append-debug "블록 정의를 찾을 수 없음")
      )
    )
  )
  texts
)

(defun dedupe-texts (entries / unique seen entry normalized)
  (setq unique '()
        seen '())
  (foreach entry entries
    (setq normalized (car entry))
    (if (and normalized (= (type normalized) 'STR) (> (strlen normalized) 0))
      (if (not (member normalized seen))
        (progn
          (setq unique (cons entry unique))
          (setq seen (cons normalized seen))
        )
      )
    )
  )
  (reverse unique)
)

(defun raw-text->ps-literal (raw / res pos chunk)
  (setq res ""
        pos 1)
  (while (<= pos (strlen raw))
    (if (and (<= (+ pos 6) (strlen raw)) (= (substr raw pos 3) "\\U+"))
      (progn
        (setq res (strcat res "\\u" (substr raw (+ pos 3) 4)))
        (setq pos (+ pos 7))
      )
      (progn
        (setq chunk (substr raw pos 1))
        (if (= chunk "'")
          (setq res (strcat res "''"))
          (setq res (strcat res chunk))
        )
        (setq pos (1+ pos))
      )
    )
  )
  (strcat "'" res "'")
)

(defun ps-quote-string (s / res pos ch)
  (setq res ""
        pos 1)
  (while (<= pos (strlen s))
    (setq ch (substr s pos 1))
    (if (= ch "'")
      (setq res (strcat res "''"))
      (setq res (strcat res ch))
    )
    (setq pos (1+ pos))
  )
  (strcat "'" res "'")
)

(defun rename-dwg-with-unicode (filepath unicode-literal / path-literal cmd)
  (setq path-literal (ps-quote-string filepath)
        cmd (strcat
               "chcp 65001>nul & powershell -NoProfile -Command \"$n = [regex]::Unescape(" unicode-literal "); Rename-Item -LiteralPath " path-literal " -NewName $n\""))
  (command "_.SHELL" cmd)
)

;; (removed delete-unicode-target helper; behavior folded into rename)

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
            (setq attr-texts (collect-insert-attribute-texts e bbox))
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

(defun collect-texts-from-selection (ss bbox / texts i e elst etype block-name)
  (setq texts '())
  (if (and ss (> (sslength ss) 0))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i)
              elst (entget e)
              etype (dxf-get 0 elst))
        (cond
          ((or (= etype "TEXT") (= etype "MTEXT"))
           (setq texts (append texts (collect-entity-text elst)))
          )
          ((= etype "INSERT")
           (setq block-name (dxf-get 2 elst))
           (setq texts (append texts (collect-insert-attribute-texts e bbox)))
           (setq texts (append texts (collect-block-definition-texts block-name)))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (dedupe-texts texts)
)



(defun append-debug (msg / fn)
  (setq fn (open (strcat (getvar "DWGPREFIX") "console.log") "a"))
  (if fn
    (progn
      (write-line msg fn)
      (close fn)
    )
  )
)

(defun sanitize-fragment (text / result i char cleaned)
  ;; Return sanitized fragment without default fallback
  (setq result ""
        i 1
        cleaned (normalize-text text))
  (if (and cleaned (> (strlen cleaned) 0))
    (progn
      (while (<= i (strlen cleaned))
        (setq char (substr cleaned i 1))
        (cond
          ((member char '("\\" "/" ":" "*" "?" "\"" "<" ">" "|" "\t" "\n" "\r"))
           (setq result (strcat result "_"))
          )
          ((= char " ")
           (setq result (strcat result "_"))
          )
          (T
           (setq result (strcat result char))
          )
        )
        (setq i (1+ i))
      )
      (setq result (trim-string result))
      ;; Remove trailing dots which Windows rejects
      (while (and (> (strlen result) 0) (= (substr result (strlen result) 1) "."))
        (setq result (substr result 1 (1- (strlen result))))
      )
    )
  )
  result
)

(defun sanitize-filename (text)
  (setq text (sanitize-fragment text))
  (if (> (strlen text) 0) text "polyline")
)

(defun create-filename-from-texts (entries / cleaned sanitized-fragments raw-fragments display-fragments entry normalized raw fragment safe-name raw-joined unicode-literal display-name)
  ;; Return list (safe-name unicode-ps-literal)
  (setq cleaned (reverse (dedupe-texts entries))
        sanitized-fragments '()
        raw-fragments '()
        display-fragments '())
  (foreach entry cleaned
    (setq normalized (car entry)
          raw (cdr entry)
          fragment (sanitize-fragment normalized))
    (if (> (strlen fragment) 0)
      (setq sanitized-fragments (append sanitized-fragments (list fragment)))
    )
    (if (> (strlen normalized) 0)
      (setq display-fragments (append display-fragments (list normalized)))
    )
    (if (and raw (= (type raw) 'STR) (> (strlen raw) 0))
      (setq raw-fragments (append raw-fragments (list (trim-string raw))))
    )
  )
  (setq safe-name "")
  (foreach fragment sanitized-fragments
    (setq safe-name (if (> (strlen safe-name) 0)
                      (strcat safe-name "_" fragment)
                      fragment))
  )
  (if (= safe-name "")
    (setq safe-name "polyline")
  )
  (if (> (strlen safe-name) 120)
    (setq safe-name (substr safe-name 1 120))
  )
  (setq raw-joined "")
  (foreach raw raw-fragments
    (setq raw-joined (if (> (strlen raw-joined) 0)
                       (strcat raw-joined "_" raw)
                       raw))
  )
  (if (= raw-joined "")
    (setq raw-joined safe-name)
  )
  (setq unicode-literal (raw-text->ps-literal raw-joined))
  (setq display-name "")
  (foreach fragment display-fragments
    (setq display-name (if (> (strlen display-name) 0)
                        (strcat display-name "_" fragment)
                        fragment))
  )
  (if (= display-name "")
    (setq display-name safe-name)
  )
  (append-debug (strcat "SAFE=" safe-name))
  (append-debug (strcat "DISPLAY=" display-name))
  (append-debug (strcat "RAW=" raw-joined))
  (list safe-name unicode-literal raw-joined display-name)
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

(defun c:DXFRECT (/ sel cnt i e folder filepath ss polyPts3d pts bbox texts base-filename block-texts textSel filename-data unicode-literal raw-label display-name center)
  (princ "\nSelect closed polylines...")
  ;; Select closed polylines regardless of color
  (if (setq sel (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
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
            ;; Calculate center point for rotation
            (setq center (bbox-center bbox))
            ;; Build polygon selection sets
            (setq polyPts3d (pts-2d->3d (ensure-closed-pts pts)))
            (setq ss (ssget "WP" polyPts3d))
            (setq textSel (ssget "CP" polyPts3d '((0 . "TEXT,MTEXT,INSERT"))))
            ;; Gather candidate texts from the selection and bounding box
            (setq texts (collect-texts-from-selection textSel bbox))
            (setq texts (append texts (get-texts-inside-bbox bbox)))
            (setq block-texts (get-texts-from-block-inside-bbox bbox))
            (setq texts (append texts block-texts))
            ;; Create base filename from texts
            (setq filename-data (create-filename-from-texts texts))
            (setq base-filename (car filename-data))
            (setq filename-data (cdr filename-data)
                  unicode-literal (if filename-data (car filename-data) "'polyline'"))
            (setq filename-data (cdr filename-data)
                  raw-label (if filename-data (car filename-data) base-filename))
            (setq filename-data (cdr filename-data)
                  display-name (if filename-data (car filename-data) raw-label))
            ;; Generate filepath with index (overwrite mode)
            (setq filepath (create-dwg-filepath folder base-filename))
            ;; Delete existing temporary file if it exists
            (delete-existing-dwg-file filepath)
            ;; 항상 경계 폴리선을 포함하여 파일이 생성되도록 보장
            (if ss
              (setq ss (ssadd e ss))
              (progn (setq ss (ssadd)) (setq ss (ssadd e ss)))
            )
            ;; 90도 회전 (중심점 기준)
            (rotate-selection-90 ss center)
            ;; DWG 파일로 저장
            (if (export-selection-to-dwg ss filepath)
              (progn
                (rename-dwg-with-unicode filepath unicode-literal)
                (princ (strcat "\nSaved: " folder display-name ".dwg"))
              )
              (princ (strcat "\nSave failed: " filepath))
            )
          )
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo closed polylines selected.")
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
  
