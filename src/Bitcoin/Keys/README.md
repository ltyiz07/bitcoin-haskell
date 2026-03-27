# Bitcoin Keys and Encodings

비트코인 생태계에서 사용되는 다양한 키(Key), 서명(Signature), 주소(Address)의 종류와 각각의 직렬화(Serialization) 및 인코딩 방식을 정리한 문서입니다.

## 1. Master Data & Backup (BIP39)

지갑의 근간이 되는 엔트로피와 복구를 위한 데이터 포맷입니다.

| 데이터 종류 | 인코딩 / 포맷 | 특징 |
| :--- | :--- | :--- |
| **Mnemonic** | BIP39 Wordlist | 12~24개의 단어 조합 (Human-readable) |
| **Seed** | 512-bit Binary (Hex) | 니모닉과 솔트를 조합하여 생성된 마스터 데이터 |

## 2. Extended Keys (HD Wallet, BIP32/44/84/86)

하나의 시드에서 계층적으로 키를 생성하기 위한 확장 포맷입니다.

| 데이터 종류 | 인코딩 (Base58Check) | 용도 (Derivation Path) |
| :--- | :--- | :--- |
| **xprv / xpub** | Legacy (Prefix: `x`) | P2PKH (m/44'/0'...) |
| **yprv / ypub** | Nested SegWit (Prefix: `y`) | P2SH-P2WPKH (m/49'/0'...) |
| **zprv / zpub** | Native SegWit (Prefix: `z`) | P2WPKH (m/84'/0'...) |

## 3. Private & Public Keys (SEC/WIF)

개별 트랜잭션의 소유권을 증명하고 검증하는 데 사용되는 키 포맷입니다.

| 데이터 종류 | 인코딩 / 포맷 | 형태 | 특징 |
| :--- | :--- | :--- | :--- |
| **Private Key** | **WIF** (Base58Check) | `5...`, `K...`, `L...` | 개인키의 유입/유출용 포맷 |
| **Public Key** | **SEC (Compressed)** | 33-byte Binary | `02` 또는 `03` 접두사 + X좌표 |
| **Public Key** | **SEC (Uncompressed)** | 65-byte Binary | `04` 접두사 + X, Y좌표 (Legacy) |
| **Taproot Key** | **X-Only** (BIP340) | 32-byte Binary | Y좌표를 생략한 Schnorr 전용 포맷 |

## 4. Signatures (DER/BIP340)

트랜잭션 데이터의 무결성을 증명하기 위한 서명 직렬화 방식입니다.

| 데이터 종류 | 인코딩 / 포맷 | 길이 | 특징 |
| :--- | :--- | :--- | :--- |
| **ECDSA Signature** | **DER** (ASN.1) | 70~72 bytes | 가변 길이, $r$과 $s$ 값을 구조적으로 포함 |
| **Schnorr Signature** | **Fixed Binary** | 64 bytes | 고정 길이, BIP340 표준, 집계 가능 |

## 5. Addresses (Base58/Bech32)

사용자 간 비트코인을 송수신하기 위한 식별자입니다.

| 주소 타입 | 인코딩 방식 | 시작 문자 | 관련 표준 |
| :--- | :--- | :--- | :--- |
| **P2PKH** (Legacy) | Base58Check | `1` | - |
| **P2SH** (Script Hash) | Base58Check | `3` | BIP16 |
| **P2WPKH** (SegWit) | **Bech32** | `bc1q` | BIP173 |
| **P2TR** (Taproot) | **Bech32m** | `bc1p` | BIP350 |

