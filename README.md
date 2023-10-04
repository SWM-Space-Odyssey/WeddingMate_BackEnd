# WeddingMate_BackEnd

## MVP별 기능 정리
[링크](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/issues/112)

## DOCUMENTS
> 💻 : PR 링크 <br>
> 🤔 : 개발 과정에서 있었던 논의<br>
> 📃 : 관련 문서
<br>

### 설계 및 개발
- [💻카카오 소셜 로그인](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/19)
- [🤔Image 전송과 데이터 전송 API 분리](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/21)
- [💻redis sub/pub를 사용한 실시간 채팅 API](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/89)
- [💻테스트를 통한 좋아요 설계 비교](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/82)

- Service 계층과 Repository
   - [🤔Service 계층에서 Repository를 호출하는 것이 옳은가?](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/22)
   - [💻순환 참조를 피하기 위해 Service 계층에서 Repository 호출 로직 분리](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/113)
- Pagination
   - [💻이미지 피드 기본 pagination](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/39)
   - [🤔Pagination 시 중복 오류 해결 방법](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/78)
   - [💻커서 기반 페이지네이션 추가](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/91)
 
- 태그 기반 검색
   - [🤔File 테이블 내의 item_id 칼럼 리팩토링](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/53)
   - [🤔Tag 테이블 리팩토링 및 검색](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/54)
   - [💻MySQL full text search 적용 및 페이지네이션 추가](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/95)

### 리팩토링
- [📃DB 테이블 복수형으로 변경](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/80)
- [💻파일 업로드 과정 리팩토링 & 파일 확장자명 및 이미지 파일 검증](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/47)
- [💻포트폴리오, 아이템 코드 테스트](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/56)
- [💻파일 API 기능별로 분류](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/59)
- [💻빌더 패턴과 파라미터 객체](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/61)
- [💻API 공통 응답 포맷 생성](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/52)

### 인프라
- [🤔AWS Lambda@edge를 사용한 이미지 리사이징 자동화](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/29)
- [💻docker에서 redis 설정하기](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/pull/25)
- [📃AWS 보안 그룹과 사설/공인 IP](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/81)
- [📃redis.conf 설정](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/17)

### 기타
- [📃프로젝트 버전 설정](https://dev-minji.tistory.com/212)
- [📃Github Hook을 이용한 Jira 티켓 번호 커밋 메시지 자동 추가](https://dev-minji.tistory.com/212)
- [📃yml 환경변수 env에 등록하기](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/13)
- [📃Github Action을 이용한 PR 라벨 등록](https://dev-minji.tistory.com/227)
- [📃개발자 추천 메모장: Obsidian](https://github.com/SWM-Space-Odyssey/WeddingMate_BackEnd/discussions/110)
