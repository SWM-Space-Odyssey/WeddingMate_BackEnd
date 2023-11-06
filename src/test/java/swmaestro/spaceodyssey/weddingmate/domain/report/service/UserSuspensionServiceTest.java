package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserAccountStatusEnum;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
class UserSuspensionServiceTest extends DummyEntity {

	private static final String REDISSON_LOCK_PREFIX = "Id:";
	Users baseUser;
	Users zeroToOneBlockUser;
	Users oneBlockUser;
	Users twoBlockUser;
	@InjectMocks
	UserSuspensionService userSuspensionService;
	private String lockname;

	@BeforeEach
	void setUp() {
		makeMockData();
	}

	@Test
	@DisplayName("[성공] BLOCK 0, REPORT 0인 유저 신고")
	void addReportCntToBaseUser() {
		lockname = REDISSON_LOCK_PREFIX + baseUser.getUserId();
		Users resultUser = userSuspensionService.addReportCnt(lockname, baseUser.getUserId());

		assertEquals(1, resultUser.getReportCnt());
		assertEquals(0, resultUser.getBlockCnt());
		assertEquals(UserAccountStatusEnum.NORMAL, resultUser.getAccountStatus());
	}

	@Test
	@DisplayName("[성공] BLOCK 0, REPORT 2인 유저 신고")
	void addReportCntToZeroToOneBlockUser() {
		lockname = REDISSON_LOCK_PREFIX + zeroToOneBlockUser.getUserId();
		Users resultUser = userSuspensionService.addReportCnt(lockname, zeroToOneBlockUser.getUserId());

		assertEquals(0, resultUser.getReportCnt());
		assertEquals(1, resultUser.getBlockCnt());
		assertEquals(UserAccountStatusEnum.SUSPENDED, resultUser.getAccountStatus());
	}

	@Test
	@DisplayName("[성공] BLOCK 1, REPORT 2인 유저 신고")
	void addReportCntToTwoBlockUser() {
		lockname = REDISSON_LOCK_PREFIX + oneBlockUser.getUserId();
		Users resultUser = userSuspensionService.addReportCnt(lockname, oneBlockUser.getUserId());

		assertEquals(0, resultUser.getReportCnt());
		assertEquals(2, resultUser.getBlockCnt());
		assertEquals(UserAccountStatusEnum.SUSPENDED, resultUser.getAccountStatus());
	}

	@Test
	@DisplayName("[성공] BLOCK 2, REPORT 2인 유저 신고")
	void addReportCntToThreeBlockUser() {
		lockname = REDISSON_LOCK_PREFIX + twoBlockUser.getUserId();
		Users resultUser = userSuspensionService.addReportCnt(lockname, twoBlockUser.getUserId());

		assertEquals(0, resultUser.getReportCnt());
		assertEquals(3, resultUser.getBlockCnt());
		assertEquals(UserAccountStatusEnum.BANNED, resultUser.getAccountStatus());
	}

	void makeMockData() {
		baseUser = newMockUser("reported");
		ReflectionTestUtils.setField(baseUser, "userId", 1L);
		ReflectionTestUtils.setField(baseUser, "email", "reported@gmail.com");
		ReflectionTestUtils.setField(baseUser, "blockCnt", 0);
		ReflectionTestUtils.setField(baseUser, "reportCnt", 0);
		ReflectionTestUtils.setField(baseUser, "accountStatus", UserAccountStatusEnum.NORMAL);

		zeroToOneBlockUser = newMockUser("zeroToOne");
		ReflectionTestUtils.setField(zeroToOneBlockUser, "userId", 2L);
		ReflectionTestUtils.setField(zeroToOneBlockUser, "email", "reported1@gmail.com");
		ReflectionTestUtils.setField(zeroToOneBlockUser, "blockCnt", 0);
		ReflectionTestUtils.setField(zeroToOneBlockUser, "reportCnt", 2);
		ReflectionTestUtils.setField(zeroToOneBlockUser, "accountStatus", UserAccountStatusEnum.NORMAL);

		oneBlockUser = newMockUser("oneBlock");
		ReflectionTestUtils.setField(oneBlockUser, "userId", 3L);
		ReflectionTestUtils.setField(oneBlockUser, "email", "reported2@gmail.com");
		ReflectionTestUtils.setField(oneBlockUser, "blockCnt", 1);
		ReflectionTestUtils.setField(oneBlockUser, "reportCnt", 2);
		ReflectionTestUtils.setField(oneBlockUser, "accountStatus", UserAccountStatusEnum.NORMAL);

		twoBlockUser = newMockUser("twoBlock");
		ReflectionTestUtils.setField(twoBlockUser, "userId", 4L);
		ReflectionTestUtils.setField(twoBlockUser, "email", "reported3@gmail.com");
		ReflectionTestUtils.setField(twoBlockUser, "blockCnt", 2);
		ReflectionTestUtils.setField(twoBlockUser, "reportCnt", 2);
		ReflectionTestUtils.setField(twoBlockUser, "accountStatus", UserAccountStatusEnum.NORMAL);
	}
}