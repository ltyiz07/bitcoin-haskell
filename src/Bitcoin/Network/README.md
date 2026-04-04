# Bitcoin Network

## Download Block Process

1. 비트코인 네트워크 탐색 (Network Discovery)

DNS Seeds 활용: 비트코인 소스코드에는 하드코딩된 여러 DNS 시드 주소(예: seed.bitcoin.sipa.be)가 있습니다. 이 주소로 일반적인 DNS 쿼리를 보내면, 현재 살아있는 비트코인 노드들의 IP 목록을 반환해 줍니다.

결과물: 연결 가능한 피어(Peer)들의 IP 주소와 포트(기본 메인넷 포트: 8333) 목록.

2. 비트코인 네트워크 연결 (Handshake & Connection)

TCP 연결: 대상 IP와 8333 포트로 소켓을 엽니다.

`version` 메시지 전송: 내 노드의 버전, 현재 시간, 지원하는 기능 등의 정보를 담은 version 메시지를 만들어 보냅니다.

`verack` 응답 대기: 상대방도 나의 version 메시지를 수락한다는 의미로 verack(Version Acknowledge) 메시지를 보내고, 상대방의 version 메시지도 나에게 보냅니다. 나 역시 상대방에게 verack을 보내면 핸드셰이크가 완료됩니다.

엔디안 주의: 메시지를 직렬화하여 네트워크 소켓으로 밀어 넣을 때 엔디안 규칙(TCP 통신 자체는 Big-Endian, 비트코인 페이로드 내부는 주로 Little-Endian)이 본격적으로 적용됩니다.

3. 블록 헤더 요청 (Request Block Header)

원하는 블록의 위치를 파악하기 위해 헤더를 먼저 요청합니다. 전체 블록(수 MB)을 무턱대고 받기 전에, 가벼운 헤더(80 Bytes)만 먼저 받아서 검증하는 것이 SPV(단순 지불 검증) 방식의 핵심입니다.

`getheaders` 메시지 전송: 내가 알고 있는 마지막 블록의 해시(Locator)를 담아 보내면, 네트워크는 그 이후의 블록 헤더들을 최대 2,000개까지 묶어서 줍니다.

`headers` 메시지 수신: 상대 노드가 80바이트짜리 헤더들의 목록을 보내줍니다.

4. 블록 요청 (Request Block)

헤더를 성공적으로 받았고, 특정 블록의 전체 트랜잭션 데이터가 필요하다면 그 블록을 요청합니다.

`getdata` 메시지 전송: 앞서 받은 헤더의 '블록 해시'를 인벤토리 벡터(Inventory Vector, 데이터 유형과 해시를 묶은 구조체)에 담아 getdata 메시지로 보냅니다.

`block` 메시지 수신: 상대방이 트랜잭션이 모두 포함된 뚱뚱한 block 메시지를 보내줍니다.

## Appendix: Technical Specifications (참고용 스펙)

모든 비트코인 네트워크 메시지는 [Message Header] + [Payload] 형태로 전송됩니다.

A. Message Header 구조 (총 24 Bytes)

모든 메시지의 맨 앞에 붙는 공통 헤더입니다.

Field           Size (Bytes)    Endian  Description

Magic Bytes     4               Little  메인넷 식별자 (0xF9BEB4D9)
Command         12              ASCII   명령어 (예: version\0\0\0\0\0)
Payload Size    4               Little  뒤따라오는 페이로드의 바이트 크기
Checksum        4               -       SHA256(SHA256(Payload))의 첫 4바이트

B. version Payload 구조

네트워크 연결(2단계) 시 보내야 하는 첫 번째 메시지의 데이터 구조입니다.

Field       Size (Bytes)        Endian  Description

Version     4                   Little  프로토콜 버전 (예: 70015)
Services    8                   Little  지원 서비스 비트필드 (예: NODE_NETWORK = 1)
Timestamp   8                   Little  현재 UNIX 타임스탬프
Addr Recv   26                  N/A     수신자 네트워크 주소 구조체
Addr From   26                  N/A     송신자(나) 네트워크 주소 구조체
Nonce       8                   Little  랜덤 값 (나 자신과 연결하는 것을 방지)
User Agent  VarInt + Str        -       클라이언트 버전 문자열 (예: /Satoshi:22.0.0/)
Start Height4                  Little  나의 현재 블록 높이 (모르면 0)
Relay       1                   -       트랜잭션 릴레이 여부 (보통 0x01 또는 0x00)

C. Haskell 구현 참고 패키지 (Cabal)

Haskell로 위 P2P 네트워크 프로세스를 구현하기 위해 .cabal 파일에 다음 라이브러리 추가가 권장됩니다.

- network: DNS 시드 쿼리(1단계 탐색) 및 TCP 소켓 통신(2단계 이후)을 위해 사용됩니다.
- time: version 메시지 페이로드에 포함될 현재 UNIX 타임스탬프를 구하는 데 사용됩니다.
- random: version 메시지에서 자신과의 연결을 방지하기 위한 랜덤 8바이트 Nonce 값을 생성하는 데 사용됩니다.

(참고: 메시지 직렬화/역직렬화 및 체크섬 계산에는 기존 프로젝트에 세팅된 cereal, crypton 패키지를 활용합니다.)
