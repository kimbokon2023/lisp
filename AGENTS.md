# DXF Split - AutoLISP DWG 분할 도구

## 개요
DXF Split은 AutoCAD에서 파란색 닫힌 폴리라인(사각형)을 자동으로 찾아서 각 사각형과 그 안의 모든 객체를 별도의 DWG 파일로 내보내는 순수 AutoLISP 도구입니다.

## 주요 기능
- 파란색 닫힌 폴리라인 자동 감지
- 각 사각형 내부의 텍스트 엔티티 추출
- 텍스트 내용을 기반으로 한 의미있는 파일명 생성
- 블록 내부의 텍스트도 처리
- CADian 호환 (VL- 함수 사용 안함)

## 명령어
- `DDDD`: 메인 명령어 (DWG 분할 실행)
- `DXFRECT`: 동일한 기능의 대체 명령어

## 기술적 특징

### 1. 순수 AutoLISP 구현
- VL- 함수를 사용하지 않아 CADian과 구버전 AutoCAD에서 호환
- 모든 문자열 처리를 순수 AutoLISP로 구현

### 2. DXF 데이터 처리
```lisp
;; DXF 그룹 코드 처리
(defun dxf-get (code elst)
  (cdr (assoc code elst))
)

;; 반복되는 DXF 그룹 코드 값 수집
(defun dxf-filter-values (code elst / r)
  (setq r '())
  (while elst
    (if (= (caar elst) code)
      (setq r (cons (cdar elst) r))
    )
    (setq elst (cdr elst))
  )
  (reverse r)
)
```

### 3. 폴리라인 꼭짓점 추출
세 가지 패턴을 지원:
- **패턴 A**: 별도의 10 (x) 및 20 (y) 코드
- **패턴 B**: 10이 점 리스트 (x y [z])를 포함
- **패턴 C**: 10과 20을 순차적으로 스캔하여 쌍을 이룸

### 4. 텍스트 엔티티 처리
```lisp
(defun get-texts-inside-bbox (bbox / texts text-ents e elst etype pt text-content i x y)
  ;; 경계 상자 내의 모든 TEXT 엔티티 찾기
  (setq texts '()
        text-ents (ssget "X" '((0 . "TEXT"))))
  ;; 좌표 추출 및 유효성 검사
  ;; DXF 코드 10이 리스트인 경우 처리
  ;; y 좌표가 nil인 경우 대안 방법 시도
)
```

### 5. 파일명 생성
- 텍스트 내용을 기반으로 파일명 생성
- 잘못된 파일명 문자를 언더스코어로 대체
- 파일명 길이 제한 (50자)

```lisp
(defun sanitize-filename (text / result i char)
  ;; 잘못된 파일명 문자 제거 또는 대체
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

## 사용법

### 1. 기본 사용
```
명령: DDDD
Select blue polylines...
엔티티 선택: [파란색 폴리라인 선택]
```

### 2. 출력 파일
- 파일은 현재 도면과 같은 폴더의 하위 폴더에 저장
- 폴더명: `[도면명]\`
- 파일명: `[텍스트내용]-[번호].dwg`

## 핵심 함수들

### 기하학적 유틸리티
- `bbox-of-pts`: 점들의 경계 상자 계산
- `point-inside-bbox`: 점이 경계 상자 내부에 있는지 확인
- `lwpoly-vertices-2d`: LWPOLYLINE 꼭짓점 추출
- `polyline-vertices-2d`: POLYLINE 꼭짓점 추출

### 텍스트 처리
- `get-texts-inside-bbox`: 경계 상자 내 텍스트 추출
- `get-texts-from-block`: 블록 내 텍스트 추출
- `get-texts-from-block-inside-bbox`: 경계 상자 내 블록의 텍스트 추출

### 파일 처리
- `ensure-dwg-folder`: DWG 폴더 생성
- `create-dwg-filepath`: 파일 경로 생성
- `export-selection-to-dwg`: 선택된 객체를 DWG로 내보내기

## 오류 처리

### 1. 좌표 처리
- DXF 코드 10이 리스트인 경우 `(car x)`, `(cadr x)`로 x, y 추출
- y 좌표가 nil인 경우 0.0으로 설정하여 "잘못된 인수 유형" 오류 방지

### 2. 문자열 처리
- `strcat` 함수 대신 개별 `princ` 함수 사용
- `rtos` 함수 호출 전 숫자 유효성 검사

### 3. 한글 문자 처리
- 한글 텍스트의 안전한 출력을 위한 검증 함수
- 파일명에서 잘못된 문자 제거

## 디버깅 기능
- 상세한 디버그 출력으로 실행 과정 추적
- 각 텍스트 엔티티의 좌표 및 내용 확인
- 경계 상자 내부/외부 텍스트 구분

## 제한사항
- 파란색 (색상 코드 5) 폴리라인만 처리
- 닫힌 폴리라인만 처리
- 최소 3개 이상의 꼭짓점이 있는 폴리라인만 처리

## 호환성
- AutoCAD 2000 이상
- CADian
- 순수 AutoLISP 환경

## 개발 정보
- 작성자: AutoLISP 개발팀
- 버전: 1.0
- 최종 수정: 2024년
- 라이선스: 자유 사용