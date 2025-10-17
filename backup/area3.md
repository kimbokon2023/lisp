## area3.lsp 기술 문서 (Pure AutoLISP Area Calculator)

### 개요
`area3.lsp`는 순수 AutoLISP(DXF 파싱 기반)으로 작성된 면적·두께·부피 계산 및 표 생성 도구입니다. `CIRCLE`, `LWPOLYLINE`, `POLYLINE`, 그리고 `LINE`들의 폐합 루프를 지원하며, 도면 내 텍스트로부터 두께(T)와 현장명(Workplace)을 자동 추출한 뒤 표를 그립니다.

### 핵심 명령
- **AAAA**: 메인 명령. 선택 → 계산 → 표 삽입 순으로 실행됩니다.
- **AREATUNIT**: 합계 면적 단위 스케일 설정(미사용 시 기본 1e-6: mm²→m² 전환용 변수이지만, 표는 mm 기준 표시).
- **AREATSET**: 선분 연결 허용 오차(`eps`) 설정.

### 지원 객체와 계산 방식
- **CIRCLE**: 반지름 r 에 대해 면적 = \(\pi r^2\)
- **LWPOLYLINE / POLYLINE**: Shoelace(끈 묶기) 공식 + Bulge(호) 영역을 더하여 정확한 면적 계산
- **LINE 루프**: 선택된 `LINE`들을 오차(`eps`) 기반으로 연결해 폐합 루프를 형성하면 Shoelace로 면적 계산

### 두께·현장명 자동 인식
- 두께(T) 텍스트 인식 규칙
  - `T`(대소문자 무관) 주변에 숫자가 직접 붙어 있거나, 공백·구분자(`=` `:` 등) 사이에 있어도 인식합니다.
  - 예: `1.2T`, `T0.8`, `10 T`, `t=3.5`
  - 복수 후보가 있을 경우 `T`에 즉시 인접한 수를 우선 사용합니다.
  - 파싱된 두께는 mm 단위로 계산되며 표에는 2자리 소수, 선행 0 강제(`.8 → 0.80`)로 표시됩니다.
- 현장명(Workplace) 인식 규칙
  - 대상 도형 경계 안(또는 경계 박스 내) `TEXT/MTEXT` 중, 두께 텍스트가 아니며 문자열이 존재하는 첫 항목을 사용합니다.

### 표 출력 규칙
- 열 구성: `No | workplace | Area (mm^2) | Thickness (mm) | Volume (mm^3)`
- 정렬: `workplace`만 좌측 정렬, 나머지 숫자 열은 모두 우측 정렬
- 숫자 포맷:
  - 행 데이터의 면적/부피: 소수 0자리, 3자리마다 콤마
  - 두께: 소수 2자리(선행 0 강제)
- 합계 행:
  - `workplace` 열에 `Total` 표기
  - 면적 합계, 부피 합계를 각 열에 표시(3자리 콤마, 소수 0자리)
  - 두께 열은 공백
- `No` 열은 표의 행 순서대로 1, 2, 3 … 자동 증가

### 사용 방법
1) 파일 로드
```lisp
APPLOAD → area3.lsp
```
2) 명령 실행
```lisp
AAAA
```
3) 객체 선택
- 계산 대상: `CIRCLE`, `LWPOLYLINE`, `POLYLINE`, `LINE`
- `TEXT/MTEXT`는 자동 탐색 대상으로 별도 선택할 필요 없음
4) 표 삽입점 지정
- 명령 프롬프트 지시에 따라 표 삽입점을 클릭합니다.

### 옵션/설정
- 연결 허용 오차 설정
```lisp
AREATSET  ; 프롬프트에 원하는 eps(실수) 입력
```
- 합계 면적 스케일(정보 표시용)
```lisp
AREATUNIT ; 1 또는 2 선택 (내부 전역 변수 *areat-total-scale*)
```
- 표 글자 높이
  - 전역 변수 `*areat-texth*` (기본 250.0)

### 내부 동작 개요(개발자 참고)
- DXF 파싱 유틸: `dxf-get`, `dxf-filter-values`
- 좌표/다각형: `lwpoly-vertices-2d`, `polyline-vertices-2d-with-bulges`, `polygon-area-shoelace`, `segment-arc-wedge-area`
- 선분 루프화: `canonicalize-segments`, `connect-lines-into-loops`
- 텍스트 판별/파싱: `is-thickness-text`, `extract-thickness-from-text`, `find-workplace-in-pts/circle`
- 표 그리기: `areat-entmake-line`, `areat-entmake-text`, `areat-entmake-text-just`, `areat-draw-table`
- 숫자 포맷: `areat-format-thk`(두께용), `areat-format-number`(콤마 삽입)

### 제약 및 주의사항
- `TEXT/MTEXT` 정렬, 회전, 특수 폰트 등에 따라 간헐적으로 인식이 어려울 수 있습니다.
- `LINE` 루프는 `eps`에 민감합니다. 꼭 닫힌 경로가 아닐 경우 루프 인식이 실패할 수 있습니다.
- 3D `POLYLINE`(Z≠0) 또는 개방된 폴리라인은 면적 계산에서 제외됩니다.

### 문제해결 가이드
- 두께가 0으로 나오면?
  - 대상 도형 경계 안에 `T`가 포함된 텍스트가 있는지, `T` 바로 인접한 숫자가 있는지 확인하세요.
- 루프가 인식되지 않으면?
  - `AREATSET`로 `eps` 값을 키운 다음 다시 시도하거나, 선 끝점 스냅이 정확히 맞는지 확인하세요.
- 표 텍스트가 겹치면?
  - 표 삽입 위치를 약간 이동하거나 `*areat-texth*` 값을 조정하세요.

### 변경 요약(최근)
- 숫자 열 우측 정렬, 3자리 콤마 적용
- 합계 행 형식: `workplace`에 `Total`, 두께 공란
- 두께 파싱 개선: `T` 인접 실수 지원, `.8 → 0.80`
- `LINE` 루프 자동 연결 및 면적 계산

### 라이선스 / 기타
- 사내용 스크립트. 필요 시 파일 복제 후 커스터마이징 가능합니다.


