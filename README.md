# Implement BTC from Bottom

## TODOs

- [x] EllipticCurve add method
    - [x] add with point at infinity
        - case: 'point at infinity' is 'identity element'
    - [x] x_1 == x_2 && y_1 == y_2 && y_1 == 0
        - case: same points and perpendicular to the x-axis and tangent to an elliptic curve
        - should return 'point at infinity'
    - [x] x_1 == x_2 && y_1 == y_2
        - case: same points
    - [x] x_1 == x_2
        - case: same x but different y
        - should return 'point at infinity'
    - [x] x_1 != x_2
        - case: different x
- [ ] Update pow method for FiniteField
- [ ] Deprecate `mkPoint` use `mkPointOnCurve`
     ✓ [Project Setup] Cabal 설정 파일 (`impl-btc.cabal`) 수정
     ✓ [Project Setup] `src/Field.hs` 파일 삭제
     ✓ [Refactor] `FiniteField` 모듈 리팩토링 (`src/Field/FiniteField.hs`)
     ✓ [Refactor] `RealField`를 `RationalField`로 리팩토링 및 이름 변경 (`src/Field/RealField.hs` -> `src/Field/RationalField.hs`)
     ✓ [Update] `EllipticCurve` 모듈 업데이트 (`src/EllipticCurve.hs`)
     ✓ [Update] `RationalFieldSpec` 테스트 업데이트 및 파일 이름 변경 (`test/RealFieldSpec.hs` -> `test/RationalFieldSpec.hs`)
     ✓ [Update] `FiniteFieldSpec` 테스트 업데이트 (`test/FiniteFieldSpec.hs`)
     ✓ [Update] `EllipticCurveSpec` 테스트 업데이트 (`test/EllipticCurveSpec.hs`)
     ✓ [Update] `test/Main.hs` 테스트 메인 파일 업데이트
     ✓ [Finalize] `cabal build`로 최종 빌드 확인
     ✓ [Finalize] `cabal test`로 최종 테스트 확인


## Refactoring Plan (from Gemini CLI)

### 1. Cabal 설정 및 프로젝트 구조 변경
*   `impl-btc.cabal` 파일 수정:
    *   `library` 섹션에 `DataKinds`, `TypeApplications`, `GeneralizedNewtypeDeriving` 등 필요한 GHC 확장 추가.
    *   `exposed-modules`에서 `Field`를 제거하고, `RealField`를 `RationalField`로 변경.
    *   `test-suite` 섹션의 `other-modules`에서 테스트 파일 이름 변경사항(`RealFieldSpec` -> `RationalFieldSpec`) 반영.
*   `src/Field.hs` 파일 삭제 (`Field` 타입클래스 제거).

### 2. `FiniteField` 리팩토링 (`src/Field/FiniteField.hs`)
*   `DataKinds`를 사용하여 `newtype FiniteField (p :: Nat) = FiniteField Integer`로 재정의.
*   `Num`과 `Fractional` 타입클래스의 인스턴스로 구현.
    *   `Num`: `KnownNat`과 `natVal`을 사용하여 모듈러 연산 구현.
    *   `Fractional`: `recip`은 `modInverse`를 사용하고 0에 대해서는 `error` 발생.
*   기존 `Field` 인스턴스 및 `mkFiniteField` 함수 제거.

### 3. `RealField` -> `RationalField` 리팩토링 및 이름 변경
*   `src/Field/RealField.hs` 파일명을 `src/Field/RationalField.hs`로 변경.
*   파일 내부에서 `RealField` 타입을 `RationalField`로 변경하고, 내부 타입을 `Double`에서 `Rational`로 교체 (`import Data.Ratio`).
*   `GeneralizedNewtypeDeriving`을 사용하여 `Num`과 `Fractional` 인스턴스 자동 유도.
*   기존 `Field` 인스턴스 및 `mkRealField` 함수 제거.

### 4. `EllipticCurve` 모듈 업데이트 (`src/EllipticCurve.hs`)
*   함수들의 제약조건을 `(Field f, Eq f)`에서 `(Fractional f, Eq f)`로 변경.
*   `add`, `sub`, `divide` 등 `Maybe`를 반환하던 함수들을 `+`, `-`, `/` 등 `Num`/`Fractional` 연산자로 교체.
*   `fromIntWith`를 `fromInteger`로 교체.
*   `Maybe`와 `do` 블록을 사용하던 로직을 `Num` 연산자를 사용하는 직접적인 계산으로 변경하여 코드 간소화.

### 5. 테스트 스위트 전체 업데이트 (`test/` 디렉토리)
*   `test/RealFieldSpec.hs` 파일을 `test/RationalFieldSpec.hs`로 이름 변경.
*   변경된 `RationalFieldSpec.hs`와 `FiniteFieldSpec.hs`에서 `Num` 연산자를 사용하여 테스트 케이스 재작성. (예: `let x :: FiniteField 223 = 192`)
*   `EllipticCurveSpec.hs`에서 `mkFiniteField`, `mkRealField` 호출을 `fromInteger` 또는 `(%)` 연산자로 교체하고, `Maybe`가 사라진 로직에 맞춰 테스트 코드 수정.
*   `test/Main.hs`에서 변경된 테스트 모듈(`RationalFieldSpec`)을 임포트하도록 수정.

### 6. 최종 빌드 및 테스트
*   `cabal build` 명령으로 전체 프로젝트가 성공적으로 컴파일되는지 확인.
*   `cabal test` 명령으로 모든 테스트가 통과하는지 확인.



