package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNameNotFoundException;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@SpringBootTest
public class ReportLockTest extends DummyEntity {

	private static final String REDISSON_LOCK_PREFIX = "Id:";

	@Autowired
	private UserSuspensionService userSuspensionService;

	@Autowired
	private UsersRepository usersRepository;

	private String lockname;
	private Users reportedUser;

	@BeforeEach
	void setUp() {
		makeMockData();
	}

	@Test
	@DisplayName("[성공/분산락] 신고 분산락 테스트")
	void test_report_with_lock() throws InterruptedException {
		lockname = REDISSON_LOCK_PREFIX + reportedUser.getUserId();

		int numberOfThreads = 5;
		ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreads);
		CountDownLatch latch = new CountDownLatch(numberOfThreads);

		for (int i = 0; i < numberOfThreads; i++) {
			executorService.submit(() -> {
				try {
					// 분산락 적용 메서드 호출
					userSuspensionService.addReportCnt(lockname, reportedUser.getUserId());
				} finally {
					latch.countDown();
				}
			});
		}
		latch.await();

		Users pUsers = usersRepository.findById(reportedUser.getUserId())
				.orElseThrow(UserNameNotFoundException::new);

		System.out.println(pUsers.getReportCnt());
		System.out.println(pUsers.getBlockCnt());
		Assertions.assertThat(pUsers.getReportCnt()).isEqualTo(numberOfThreads % 3);
		Assertions.assertThat(pUsers.getBlockCnt()).isEqualTo(numberOfThreads / 3);
	}

	void makeMockData() {
		reportedUser = newMockUser("reported");
		usersRepository.save(reportedUser);
	}
}
