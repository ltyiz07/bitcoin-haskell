# Revision history for impl-btc

## [VERSION] -- YYYY-mm-dd

### Added

### Changed

### Fixed

## [0.1.5] -- 2026-03-23

### Added

- Add sign method

## [0.1.4] -- 2026-03-22

### Added

- Add hash methods
  - HASH-256
  - HASH-160
- Add hmac method
  - HMAC SHA-1

## [0.1.3] -- 2026-03-21

### Added

- Add scalar multiply point method

## [0.1.2] -- 2026-03-20

### Changed

- Refactor Field, EllipticCurve classes
  - [Project Setup] Cabal 설정 파일 (`impl-btc.cabal`) 수정
  - [Project Setup] `src/Field.hs` 파일 삭제
  - [Refactor] `FiniteField` 모듈 리팩토링 (`src/Field/FiniteField.hs`)
  - [Refactor] `RealField`를 `RationalField`로 리팩토링 및 이름 변경 (`src/Field/RealField.hs` -> `src/Field/RationalField.hs`)
  - [Update] `EllipticCurve` 모듈 업데이트 (`src/EllipticCurve.hs`)
  - [Update] `RationalFieldSpec` 테스트 업데이트 및 파일 이름 변경 (`test/RealFieldSpec.hs` -> `test/RationalFieldSpec.hs`)
  - [Update] `FiniteFieldSpec` 테스트 업데이트 (`test/FiniteFieldSpec.hs`)
  - [Update] `EllipticCurveSpec` 테스트 업데이트 (`test/EllipticCurveSpec.hs`)
  - [Update] `test/Main.hs` 테스트 메인 파일 업데이트


## [0.1.1] -- 2026-02-05

### Added

- Field classes with interface
- EllipticCurve class
- Test codes

## [0.1.0] -- 2026-02-03

- Init project. Supporting book isbn - “Programming Bitcoin by Jimmy Song (O’Reilly). Copyright 2019 Jimmy Song, 978-1-492-03149-9.”

### Added

- Initialize git

