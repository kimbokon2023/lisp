# AutoCAD LISP 텍스트 처리 기술문서

## 개요
이 문서는 AutoCAD LISP에서 VL- 함수 없이 순수한 AutoLISP 코드로 텍스트를 처리하는 방법을 설명합니다. CADian용 LISP 파일 작성에 적합한 기법들을 다룹니다.

## 1. 기본 텍스트 추출

### 1.1 DXF 코드를 이용한 텍스트 정보 추출
```lisp
;; DXF 코드에서 특정 값 추출
(defun dxf-get (code elst)
  (cdr (assoc code elst))
)

;; 텍스트 엔티티에서 기본 정보 추출
(defun get-text-info (ent / elst)
  (setq elst (entget ent))
  (list
    (dxf-get 1 elst)    ; 텍스트 내용
    (dxf-get 10 elst)   ; 삽입점 (X, Y, Z)
    (dxf-get 40 elst)   ; 문자 높이
    (dxf-get 50 elst)   ; 회전 각도
    (dxf-get 7 elst)    ; 문자 스타일
    (dxf-get 8 elst)    ; 도면층
    (dxf-get 62 elst)   ; 색상
  )
)
```

### 1.2 모든 텍스트 엔티티 검색
```lisp
;; 도면의 모든 TEXT 엔티티 검색
(defun get-all-texts (/ ss texts i ent elst text-content)
  (setq texts '())
  (if (setq ss (ssget "X" '((0 . "TEXT"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              elst (entget ent)
              text-content (dxf-get 1 elst))
        (if text-content
          (setq texts (cons text-content texts))
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse texts)
)
```

## 2. 경계 상자 내 텍스트 검색

### 2.1 경계 상자 생성
```lisp
;; 점들의 경계 상자 계산
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
```

### 2.2 점이 경계 상자 내에 있는지 확인
```lisp
;; 점이 경계 상자 내에 있는지 확인
(defun point-inside-bbox (pt bbox)
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
```

### 2.3 경계 상자 내 텍스트 검색
```lisp
;; 경계 상자 내의 모든 텍스트 검색
(defun get-texts-inside-bbox (bbox / texts text-ents e elst etype pt text-content i x y)
  (setq texts '()
        text-ents (ssget "X" '((0 . "TEXT"))))
  (if text-ents
    (progn
      (setq i 0)
      (while (< i (sslength text-ents))
        (setq e (ssname text-ents i)
              elst (entget e)
              etype (dxf-get 0 elst))
        (if (= etype "TEXT")
          (progn
            (setq x (dxf-get 10 elst)
                  y (dxf-get 20 elst)
                  text-content (dxf-get 1 elst))
            (if (and (numberp x) (numberp y) text-content)
              (progn
                (setq pt (list x y))
                (if (point-inside-bbox pt bbox)
                  (setq texts (cons text-content texts))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse texts)
)
```

## 3. 텍스트 분류 및 처리

### 3.1 텍스트 분류 함수
```lisp
;; 두께 텍스트인지 확인 (T 주변에 숫자가 있는지)
(defun is-thickness-text (txt)
  (and txt
       (wcmatch (strcase txt) "*T*")
       (> (strlen (extract-adjacent-number-around-T txt)) 0)))

;; 작업장 텍스트인지 확인 (두께 텍스트가 아닌 것)
(defun is-workplace-text (txt)
  (and txt (not (is-thickness-text txt))))
```

### 3.2 숫자 추출 함수
```lisp
;; T 주변의 숫자 추출
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

;; 숫자와 점만 추출
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
```

## 4. 파일명 생성

### 4.1 파일명 정리 함수
```lisp
;; 파일명에 사용할 수 없는 문자를 언더스코어로 변경
(defun sanitize-filename (text / result i char)
  (setq result ""
        i 1)
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
  result
)
```

### 4.2 텍스트 목록에서 파일명 생성
```lisp
;; 텍스트 목록으로부터 파일명 생성
(defun create-filename-from-texts (texts / filename)
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
    "area"  ; 기본값
  )
)
```

## 5. 블록 내 텍스트 처리

### 5.1 블록 내 텍스트 추출
```lisp
;; 블록 내의 모든 텍스트 추출
(defun get-texts-from-block (block-name / texts block-ents e elst etype x y text-content)
  (setq texts '()
        block-ents (tblsearch "BLOCK" block-name))
  (if block-ents
    (progn
      (setq e (cdr (assoc -2 block-ents)))
      (while e
        (setq elst (entget e)
              etype (dxf-get 0 elst))
        (if (= etype "TEXT")
          (progn
            (setq x (dxf-get 10 elst)
                  y (dxf-get 20 elst)
                  text-content (dxf-get 1 elst))
            (if (and (numberp x) (numberp y) text-content)
              (setq texts (cons text-content texts))
            )
          )
        )
        (setq e (entnext e))
      )
    )
  )
  (reverse texts)
)
```

### 5.2 경계 상자 내 블록의 텍스트 추출
```lisp
;; 경계 상자 내 블록들의 텍스트 추출
(defun get-texts-from-block-inside-bbox (bbox / texts block-ents e elst etype block-name block-texts i x y pt)
  (setq texts '()
        block-ents (ssget "X" '((0 . "INSERT"))))
  (if block-ents
    (progn
      (setq i 0)
      (while (< i (sslength block-ents))
        (setq e (ssname block-ents i)
              elst (entget e)
              etype (dxf-get 0 elst))
        (if (= etype "INSERT")
          (progn
            (setq x (dxf-get 10 elst)
                  y (dxf-get 20 elst)
                  block-name (dxf-get 2 elst))
            (if (and (numberp x) (numberp y) block-name)
              (progn
                (setq pt (list x y))
                (if (point-inside-bbox pt bbox)
                  (progn
                    ;; Get texts from this block
                    (setq block-texts (get-texts-from-block block-name))
                    (setq texts (append texts block-texts))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  texts
)
```

## 6. 유틸리티 함수

### 6.1 문자열 유틸리티
```lisp
;; 빈 문자열이 아닌지 확인
(defun non-empty-str (s)
  (and s (= (type s) 'STR) (> (strlen s) 0)))

;; 값을 문자열로 변환
(defun ensure-string (v)
  (cond
    ((and v (= (type v) 'STR)) v)
    ((numberp v) (rtos v 2 6))
    (T "")
  )
)
```

### 6.2 디버깅 함수
```lisp
;; 텍스트 검색 디버깅
(defun debug-text-search (bbox / texts text-ents e elst etype pt text-content i x y)
  (setq texts '()
        text-ents (ssget "X" '((0 . "TEXT"))))
  (princ (strcat "\n[DEBUG] bbox: " (if bbox (strcat "(" (rtos (car (car bbox)) 2 2) "," (rtos (cadr (car bbox)) 2 2) ") to (" (rtos (car (cadr bbox)) 2 2) "," (rtos (cadr (cadr bbox)) 2 2) ")") "nil")))
  (if text-ents
    (progn
      (princ (strcat "\n[DEBUG] found " (itoa (sslength text-ents)) " TEXT entities"))
      (setq i 0)
      (while (< i (sslength text-ents))
        (setq e (ssname text-ents i)
              elst (entget e)
              etype (dxf-get 0 elst))
        (if (= etype "TEXT")
          (progn
            (setq x (dxf-get 10 elst)
                  y (dxf-get 20 elst)
                  text-content (dxf-get 1 elst))
            (if (and (numberp x) (numberp y) text-content)
              (progn
                (setq pt (list x y))
                (princ (strcat "\n[DEBUG] text: '" text-content "' at (" (rtos x 2 2) "," (rtos y 2 2) ")"))
                (if (point-inside-bbox pt bbox)
                  (progn
                    (princ " - INSIDE bbox")
                    (setq texts (cons text-content texts))
                  )
                  (princ " - OUTSIDE bbox")
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
    (princ "\n[DEBUG] no TEXT entities found")
  )
  (princ (strcat "\n[DEBUG] collected texts: " (if texts (strcat "(" (apply 'strcat (mapcar '(lambda (t) (strcat t " ")) texts)) ")") "none")))
  (reverse texts)
)
```

## 7. 실제 사용 예제

### 7.1 파란색 사각형 내 텍스트로 파일명 생성
```lisp
(defun c:DXFRECT (/ sel cnt i e folder filepath ss polyPts3d pts bbox texts base-filename block-texts)
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
            ;; Generate filepath with index
            (setq filepath (create-dwg-filepath folder base-filename (1+ i)))
            ;; ... 파일 저장 로직 ...
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (princ)
)
```

## 8. 주의사항

### 8.1 CADian 호환성
- VL- 함수 사용 금지
- 순수한 AutoLISP 함수만 사용
- 최신 AutoCAD 기능 사용 금지

### 8.2 성능 고려사항
- 대용량 도면에서 텍스트 검색 시 성능 저하 가능
- 경계 상자 검색 시 적절한 필터링 필요
- 불필요한 반복문 최소화

### 8.3 오류 처리
- 텍스트 데이터 유효성 검사 필수
- 경계 상자 범위 검증
- 파일명 생성 시 특수문자 처리

## 9. 결론

이 문서에서 제시한 방법들을 사용하면 CADian 환경에서도 효과적으로 텍스트를 처리할 수 있습니다. 순수한 AutoLISP 코드로 작성되어 호환성 문제가 없으며, 실제 프로젝트에서 바로 사용할 수 있는 실용적인 기법들입니다.

VL- 함수 제거: vl-string-search → string-find-dot (순수 AutoLISP)
VL- 함수 제거: vl-prin1-to-string → 안전한 문자열 처리
한글 텍스트 검증: is-valid-text-content 함수 추가
안전한 텍스트 출력: safe-print-text 함수 추가
파일명 생성 개선: 유효하지 않은 텍스트 처리
이제 다시 DDDD 명령어를 실행해보세요. 한글 텍스트가 제대로 인식되고 파일명으로 사용될 것입니다.
주요 개선점:
한글 깨짐 방지: 텍스트 유효성 검사 추가
안정성 향상: VL- 함수 제거로 CADian 호환성 확보
디버깅 개선: 안전한 텍스트 출력으로 문제 진단 용이
오류 처리: 유효하지 않은 텍스트에 대한 적절한 처리