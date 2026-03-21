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
- [x] Update pow method for FiniteField
- [ ] Deprecate `mkPoint` use `mkPointOnCurve`
- [x] Refactor Fields and EllipticCurve classes
- [ ] Create secp256k1 curve
- [ ] Transaction bit parsing
    - [ ] Key serialization
        - [ ] SEC
        - [ ] DER
        - [ ] WIF
    - [ ] Transaction serialization
    - [ ] Script parsing
- [ ] Block
- [ ] Networking


## How to test code

- `cabal build` 명령으로 전체 프로젝트가 성공적으로 컴파일되는지 확인.
- `cabal test` 명령으로 모든 테스트가 통과하는지 확인.



