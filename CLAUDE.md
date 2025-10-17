# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains AutoLISP utilities for AutoCAD/CADian automation, specifically focused on DWG file manipulation and geometric operations. The primary tool is DXF Split, which splits AutoCAD drawings into multiple DWG files based on closed polylines.

## Language: AutoLISP

**Pure AutoLISP Implementation**
- This codebase uses **pure AutoLISP** without VL- functions for maximum compatibility
- Target platforms: AutoCAD 2000+, CADian, and older CAD systems
- Character encoding: UTF-8 for Korean text support
- All string operations must use native AutoLISP functions (no Visual LISP extensions)

## Core Architecture

### Main Program: dxfsplit_ver03.lsp

The program follows this architectural flow:

```
User Selection (c:DXFRECT / c:DDDD)
    ↓
Polyline Analysis (get-poly-pts-2d, is-poly-closed)
    ↓
Bounding Box Calculation (bbox-of-pts)
    ↓
Text Extraction (get-texts-inside-bbox, get-texts-from-block-inside-bbox)
    ↓
Filename Generation (create-filename-from-texts, sanitize-filename)
    ↓
Export to DWG (export-selection-to-dwg)
    ↓
Unicode Filename Handling (rename-dwg-with-unicode via PowerShell)
```

### Key Functional Areas

1. **DXF Data Processing** (`dxf-get`, `dxf-filter-values`)
   - Extracts data from entity association lists
   - Handles repeated DXF group codes

2. **Polyline Vertex Extraction** (`lwpoly-vertices-2d`, `polyline-vertices-2d`)
   - Supports three vertex data patterns:
     - Pattern A: Separate DXF codes 10 (x) and 20 (y)
     - Pattern B: DXF code 10 contains point list (x y [z])
     - Pattern C: Sequential scan pairing 10 then 20
   - Handles both LWPOLYLINE and legacy POLYLINE entities

3. **Text Processing** (`collect-entity-text`, `normalize-text`)
   - Extracts text from TEXT, MTEXT, ATTRIB, ATTDEF entities
   - Handles text inside blocks (INSERT entities)
   - Processes Unicode escapes and paragraph breaks (\P, \n)
   - Deduplicates text entries

4. **Filename Sanitization** (`sanitize-filename`, `sanitize-fragment`)
   - Removes invalid Windows filename characters
   - Handles Korean characters via Unicode
   - Limits filename length to 120 characters
   - Uses PowerShell for Unicode-safe renaming

5. **Geometric Utilities**
   - `bbox-of-pts`: Calculates bounding box from point list
   - `point-inside-bbox`: Tests if point is inside bounding box
   - `nearly-equal-pt2d`: Floating-point comparison with epsilon tolerance

## Development Commands

### Testing
- Load in AutoCAD: Drag `.lsp` file into drawing window or use `APPLOAD` command
- Run commands: `DDDD` or `DXFRECT`
- Debug output: Check `console.log` in drawing directory (via `append-debug` function)

### No Build System
- Pure AutoLISP requires no compilation
- No package manager or dependencies
- Direct loading into CAD application

## Critical Implementation Constraints

### String Operations
- **Never use VL- functions** (vl-string-*, vl-filename-*, etc.)
- Use native functions: `strcat`, `substr`, `strlen`, `wcmatch`
- Manual iteration for string processing (no mapcar on strings)

### Coordinate Extraction
- Always check if DXF code 10/20 returns number or list
- Handle nil coordinates with defensive programming
- Use `(car x)`, `(cadr x)` for list-based coordinates

### Unicode and Korean Text
- **Pure AutoLISP Unicode decoder** implemented (no VL- functions)
- Supports multiple Unicode patterns:
  - `\U+XXXX`: Standard Unicode escape (e.g., `\U+D55C` → 한)
  - `\M+nXXXX`: MIF encoding (legacy CAD systems)
  - Direct UTF-8 strings
- Manual hex-to-decimal conversion for Unicode code points
- Use PowerShell with UTF-8 encoding for file operations
- Shell commands must start with `chcp 65001>nul` for UTF-8
- Escape single quotes in PowerShell strings as `''`
- Use `raw-text->ps-literal` for Unicode escape conversion

### File Operations
- All file paths must use absolute paths
- Use `WBLOCK` command for DWG export (not SAVEAS)
- Set EXPERT system variable to 2 to suppress prompts
- Create output folder: `[drawing-directory]\[drawing-name]\`

## Code Patterns

### DXF Entity Data Access
```lisp
(setq elst (entget entity-name))
(setq value (dxf-get 10 elst))  ; Get single value
(setq values (dxf-filter-values 10 elst))  ; Get all values for code
```

### Safe Coordinate Extraction
```lisp
(setq c10 (dxf-get 10 elst))
(cond
  ((numberp c10) c10)
  ((listp c10) (car c10))
  (T nil)
)
```

### Unicode Decoding (Pure AutoLISP with UTF-8 Encoding)
```lisp
;; Hex character to integer
(defun hex-char-to-int (c)
  (setq a (ascii c))
  (cond
    ((and (>= a 48) (<= a 57)) (- a 48))   ; 0-9
    ((and (>= a 65) (<= a 70)) (- a 55))   ; A-F
    ((and (>= a 97) (<= a 102)) (- a 87))  ; a-f
    (T 0)))

;; Convert \U+XXXX to character via UTF-8 bytes
;; CADian's chr only supports 0-255, so Unicode must be UTF-8 encoded
(setq hex-code "D55C")              ; 한
(setq unicode-val (hex-to-int hex-code))  ; 54620
;; Convert to UTF-8: 54620 → 0xED95 9C → chr(237)+chr(149)+chr(156)
(setq korean-char (unicode-to-utf8-string unicode-val))

;; UTF-8 encoding for 3-byte range (covers Korean)
;; U+0800 - U+FFFF: 1110xxxx 10xxxxxx 10xxxxxx
(setq b1 (+ 224 (/ code 4096)))        ; 1110xxxx
(setq b2 (+ 128 (rem (/ code 64) 64))) ; 10xxxxxx
(setq b3 (+ 128 (rem code 64)))        ; 10xxxxxx
(strcat (chr b1) (chr b2) (chr b3))
```

### Text Normalization Pipeline
```lisp
raw-text → replace-par-breaks → decode-unicode-escapes → trim-string → normalized-text
```

### Shell Command Pattern
```lisp
(setq cmd (strcat "chcp 65001>nul & [command]"))
(command "_.SHELL" cmd)
```

## Version History

- **backup/area*.lsp**: Legacy area calculation utilities (deprecated)
- **backup/dxfsplit*.lsp**: Evolution of DXF split functionality
- **dxfsplit_ver03.lsp**: Current production version

## Important Notes

1. **Epsilon Tolerance**: Controlled by `*dxfsplit-eps*` (default 1e-6) for geometric comparisons
2. **Closed Polylines**: Checked via DXF flag 70 or first/last point comparison
3. **Minimum Vertices**: Polylines must have ≥3 vertices to process
4. **Block Text Extraction**: Handles both ATTRIB (instance) and ATTDEF (definition) text
5. **WBLOCK Selection**: Always includes boundary polyline to ensure file creation

## Korean Language Support

All user-facing strings and documentation are in Korean. When modifying:
- Maintain Korean comments where they exist
- Use UTF-8 encoding for all file saves
- Test Korean filename generation thoroughly
- Verify PowerShell Unicode handling

## Debugging

Debug output is written to `console.log` in the drawing directory:
```lisp
(append-debug "Debug message here")
```

### Debug Log Analysis

**Text Extraction Logs:**
```
=== TEXT 엔티티 텍스트 추출 ===
DXF(1) 원본: \U+D55C\U+AE00        ← Unicode escape sequence
Par-breaks 처리 후: \U+D55C\U+AE00  ← After paragraph break processing
Normalize 후: 한글                   ← Final decoded Korean text
```

**Block Text Logs:**
```
=== 블록 정의 텍스트 추출: BLOCK_NAME ===
블록 정의 찾음
블록 내 ATTDEF 엔티티 #1
DXF(1) 원본: \U+C81C\U+BAA9
Normalize 후: 제목
블록 내 총 텍스트 엔티티: 1
```

**Filename Generation Logs:**
```
SAFE=한글파일명              ← Sanitized filename
DISPLAY=한글파일명          ← Display name
RAW=\U+D55C\U+AE00...      ← Raw Unicode escapes
Saved: C:\path\한글파일명.dwg
```

**Trace Operations:**
- Polyline selection and validation
- Text extraction (TEXT, MTEXT, ATTRIB, ATTDEF)
- Block definition text retrieval
- Unicode escape decoding
- Filename generation and sanitization
- File export and renaming
