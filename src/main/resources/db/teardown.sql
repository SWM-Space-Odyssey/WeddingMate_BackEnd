-- MockMvc (컨트롤러) 테스트에서만 사용
-- 외래 키 제약 조건 무시
SET FOREIGN_KEY_CHECKS = 0;

-- 테이블 비우기 또는 초기화
TRUNCATE TABLE items;
TRUNCATE TABLE users;
TRUNCATE TABLE portfolios;

-- 외래 키 제약 조건 복구
SET FOREIGN_KEY_CHECKS = 1;
