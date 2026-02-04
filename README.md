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
