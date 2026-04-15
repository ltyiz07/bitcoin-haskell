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

- [x] Setup & Utilities
  - [x] Add `cereal`, `base58-bytestring` to `.cabal` (add `network` later)
  - [x] Create `src/Bitcoin` directory structure
  - [x] Implement modular square root function in `src/ECDSA/Field/FiniteField.hs`
  - [x] Implement `src/Bitcoin/Utils/VarInt.hs` for `VarInt` serialization/deserialization
  - [x] Implement other utility files in `src/Utils/` as needed (e.g., for endianness, specific byte manipulations)
   
- [x] Key Serialization
  - [x] SEC Format
    - [x] Implement SEC encoding (compressed & uncompressed)
    - [x] Implement SEC decoding
  - [x] WIF Format
    - [x] Implement Base58Check encoding
    - [x] Implement WIF encoding/decoding
  - [x] DER Format
    - [x] Implement DER encoding for signatures

- [x] Transaction Serialization
  - [x] Define data types (`Transaction`, `TxIn`, `TxOut`, `Script`)
  - [x] Implement `Binary` instance for `Script` (using `VarInt`)
  - [x] Implement `Binary` instances for `TxOut`, `TxIn`, etc.
  - [x] Implement `Binary` instance for `Transaction`
    - [x] Support legacy transaction format
    - [x] Support SegWit transaction format (marker & witness data)

- [ ] Block Serialization
  - [x] Define data `BlockHeader` types
  - [ ] Define data `Block` types
  - [ ] Implement Merkle Root calculation

- [ ] Networking (P2P)
  - [x] Define network message data types (`MessageHeader`, etc.)
  - [x] Implement `Binary` instance for `MessageHeader`
  - [x] Implement handshake logic (`version`, `verack`)
  - [ ] Implement data exchange logic (`inv`, `getdata`, `tx`, `block`)

- [ ] Advanced Features
  - [ ] Implement full Script evaluation engine
  - [ ] Support Taproot (Schnorr signatures, P2TR outputs)


## How to test code

- `cabal build` 명령으로 전체 프로젝트가 성공적으로 컴파일되는지 확인.
- `cabal test` 명령으로 모든 테스트가 통과하는지 확인.

### Verify test
```
cabal test --test-options="--match \"Verify bitcoin block\""
cabal test --test-options="--match \"Verify bitcoin block\" +RTS -s -RTS"
```


## Module add sequence

1. Add source-code file
  - add `.hs` source file under `src/` directory
  - edit `.cabal`
    - append source-code module at `library.exposed-modules`
2. Add test-code file
  - add .hs test file under `test/` directory
  - import test spec module from `test/Main.hs` and add hspec item
  - edit `.cabal`
    - append test-code module at `"test-suite impl-btc-test".other-modules`

## Network connection


## Sample data


### Block

```
[{
  "hash": "00000000000000000001af450ebf3720f613c5c84cc9fa5f573305b04dc39020",
  "size": "1736139",
  "stripped_size": "752375",
  "weight": "3993264",
  "number": "942635",
  "version": "537321472",
  "merkle_root": "c9e9d4d28a46c6f2cb519b8224071d28e85c5b381cf54ad887f8232ab27883d6",
  "timestamp": "2026-03-28 12:34:45.000000 UTC",
  "timestamp_month": "2026-03-01",
  "nonce": "51db33c7",
  "bits": "17021a91",
  "coinbase_param": "032b620e04e5cac7692f466f756e6472792055534120506f6f6c202364726f70676f6c642ffabe6d6d9f8f3445dd482c3c8b80a0ba0228e32ef61b2a76c1157ccfa5a13622f8a275d9010000000000000047d62a4b07a3010000000000",
  "transaction_count": "2905"
}]
```

### Transaction

```
[{
  "hash": "ae8b9dce43203d3f4e1062f180a5dc121f30609a99732ad13bfd2609b96d960a",
  "size": "432",
  "virtual_size": "270",
  "version": "2",
  "lock_time": "0",
  "block_hash": "00000000000000000001af450ebf3720f613c5c84cc9fa5f573305b04dc39020",
  "block_number": "942635",
  "block_timestamp": "2026-03-28 12:34:45.000000 UTC",
  "block_timestamp_month": "2026-03-01",
  "input_count": "2",
  "output_count": "4",
  "input_value": "16966713",
  "output_value": "16965900",
  "is_coinbase": "false",
  "fee": "813",
  "inputs": [{
    "index": "0",
    "spent_transaction_hash": "d871a3623ff03421be290220968adc623a4509ff9ac768ce34f8d7cc48fff192",
    "spent_output_index": "1",
    "script_asm": "",
    "script_hex": "",
    "sequence": "4294967293",
    "required_signatures": null,
    "type": "witness_v0_keyhash",
    "addresses": ["bc1qll3jehqv9qwt2wtrnmkx99mur25tgwlkpzw06h"],
    "value": "200000"
  }, {
    "index": "1",
    "spent_transaction_hash": "f4ff9897538960ef04793b6ef2b027bd959e7652df65ce12d18d58bef77c2607",
    "spent_output_index": "22",
    "script_asm": "",
    "script_hex": "",
    "sequence": "4294967293",
    "required_signatures": null,
    "type": "witness_v0_keyhash",
    "addresses": ["bc1qj42nydh9gauwyx3ta0elwendclwavetus65rtx"],
    "value": "16766713"
  }],
  "outputs": [{
    "index": "0",
    "script_asm": "0 a5760be838efd97c65017977e6c48ac8b43389dd",
    "script_hex": "0014a5760be838efd97c65017977e6c48ac8b43389dd",
    "required_signatures": null,
    "type": "witness_v0_keyhash",
    "addresses": ["bc1q54mqh6pcalvhcegp09m7d3y2ez6r8zwa35pht7"],
    "value": "500000"
  }, {
    "index": "1",
    "script_asm": "0 f11789b05aaee3bbf124c493ca456ae3ff7dfe18",
    "script_hex": "0014f11789b05aaee3bbf124c493ca456ae3ff7dfe18",
    "required_signatures": null,
    "type": "witness_v0_keyhash",
    "addresses": ["bc1q7ytcnvz64m3mhufycjfu53t2u0lhmlscz85gz0"],
    "value": "500000"
  }, {
    "index": "2",
    "script_asm": "0 3d433bf051470ae73abeaea5a1cdf61fdb9b3242",
    "script_hex": "00143d433bf051470ae73abeaea5a1cdf61fdb9b3242",
    "required_signatures": null,
    "type": "witness_v0_keyhash",
    "addresses": ["bc1q84pnhuz3gu9www4746j6rn0krldekvjzm0hd2p"],
    "value": "15465900"
  }, {
    "index": "3",
    "script_asm": "0 bc4f6b38ec77af76e2080b0e7d0733b2bba27aff",
    "script_hex": "0014bc4f6b38ec77af76e2080b0e7d0733b2bba27aff",
    "required_signatures": null,
    "type": "witness_v0_keyhash",
    "addresses": ["bc1qh38kkw8vw7hhdcsgpv886penk2a6y7hlaqtgk3"],
    "value": "500000"
  }]
}]
```

### Transaction Hex

```
0200000000010292f1ff48ccd7f834ce68c79aff09453a62dc8a96200229be2134f03f62a371d80100000000fdffffff07267cf7be588dd112ce65df52769e95bd27b0f26e3b7904ef6089539798fff41600000000fdffffff0420a1070000000000160014a5760be838efd97c65017977e6c48ac8b43389dd20a1070000000000160014f11789b05aaee3bbf124c493ca456ae3ff7dfe18acfdeb00000000001600143d433bf051470ae73abeaea5a1cdf61fdb9b324220a1070000000000160014bc4f6b38ec77af76e2080b0e7d0733b2bba27aff0247304402206aacf8e1cf33fada414f642c4edf909f29c6eac71cd9d1b3f05a7c2afe75839402201275465d1b47d23e0a7690f8d3c2e39bb845f10f0149fe5e64ccadc465e68878012102a95eec718942d730689fc78301d356f9f96db7e87514e20bebc80e4d2e64f876024730440220019937ee0e9215696b9c224acbe0d01b2ef12d6a5656460a34f41d71ac14d33d022076c338900c7a1a81e103ebbbbbbf236a369b422dabb1edfae383026d8d1d71a80121031856628a41cb111b05efb23ef8e5026536275846acfcef8fe463d3e313e3fc1900000000

020000000001018a7f4e7578cfbac69328abf34b882b4b6d5de538f30fbabf164c2377d1393d722600000000fdffffff02df83010000000000160014368f1afd189a5488eb5cf94c8f13ee29a3a0d9b9400d030000000000160014ffe32cdc0c281cb539639eec62977c1aa8b43bf602473044022017722420e7b7611af8da5844664f7b1a5251b09ee885290a9754bbba371a9e8302200d72b863729d43f9cab843ae37dc9efbdb1f72665e82117a189bcccb6f42a4e101210301f457faf6aef1b4904e515ef5c71be61843ae2fc8f834afb761625f239e575d00000000
```

