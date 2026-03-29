# Implement BTC from Bottom

## TODOs

### ECDSA

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
- [x] Deprecate `mkPoint` use `mkPointOnCurve`
- [x] Refactor Fields and EllipticCurve classes
- [x] Create secp256k1 curve
- [x] Add hash methods
- [x] Implement determine-k method

### Bitcoin Protocol

- [ ] Setup & Utilities
  - [ ] Add `cereal`, `base58-bytestring` to `.cabal` (add `network` later)
  - [ ] Create `src/Bitcoin` directory structure (including `src/Bitcoin/Utils/` directory)
  - [ ] Implement modular square root function in `src/ECDSA/Field/FiniteField.hs`
  - [ ] Implement `src/Bitcoin/Utils/VarInt.hs` for `VarInt` serialization/deserialization
  - [ ] Implement other utility files in `src/Bitcoin/Utils/` as needed (e.g., for endianness, specific byte manipulations)
   
- [ ] Key Serialization
  - [x] SEC Format
    - [x] Implement SEC encoding (compressed & uncompressed)
    - [x] Implement SEC decoding
  - [ ] WIF Format
    - [ ] Implement Base58Check encoding
    - [ ] Implement WIF encoding/decoding
  - [ ] DER Format
    - [ ] Implement DER encoding for signatures

- [ ] Transaction Serialization
  - [ ] Define data types (`Transaction`, `TxIn`, `TxOut`, `Script`)
  - [ ] Implement `Binary` instance for `Script` (using `VarInt`)
  - [ ] Implement `Binary` instances for `TxOut`, `TxIn`, etc.
  - [ ] Implement `Binary` instance for `Transaction`
    - [ ] Support legacy transaction format
    - [ ] Support SegWit transaction format (marker & witness data)

- [ ] Block Serialization
  - [ ] Define data types (`BlockHeader`, `Block`)
  - [ ] Implement Merkle Root calculation
  - [ ] Implement `Binary` instance for `BlockHeader`
  - [ ] Implement `Binary` instance for `Block`

- [ ] Networking (P2P)
  - [ ] Define network message data types (`MessageHeader`, etc.)
  - [ ] Implement `Binary` instance for `MessageHeader`
  - [ ] Implement handshake logic (`version`, `verack`)
  - [ ] Implement data exchange logic (`inv`, `getdata`, `tx`, `block`)

- [ ] Advanced Features
  - [ ] Implement full Script evaluation engine
  - [ ] Support Taproot (Schnorr signatures, P2TR outputs)


## How to test code

- `cabal build` 명령으로 전체 프로젝트가 성공적으로 컴파일되는지 확인.
- `cabal test` 명령으로 모든 테스트가 통과하는지 확인.

## Module add sequence

1. Add source-code file
  - add `.hs` source file under `src/` directory
  - edit `.cabal`
    - append source-code module at `library.exposed-modules`
2. Add test-code file
  - add .hs test file under `test/` directory
  - import test spec module from `test/Main.hs`
  - edit `.cabal`
    - append test-code module at `"test-suite impl-btc-test".other-modules`
