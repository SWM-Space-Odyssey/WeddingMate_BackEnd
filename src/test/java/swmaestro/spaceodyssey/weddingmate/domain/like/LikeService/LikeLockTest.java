package swmaestro.spaceodyssey.weddingmate.domain.like.LikeService;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import swmaestro.spaceodyssey.weddingmate.domain.like.service.PortfolioLikeImpl;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;

@SpringBootTest
public class LikeLockTest extends DummyEntity {

	@Autowired
	private PortfolioLikeImpl portfolioLike;

	@Autowired
	private UsersRepository usersRepository;

	@Autowired
	private PortfoliosRepository portfoliosRepository;

	private Users users;

	private Portfolios portfolios;


	@BeforeEach
	void setup() {
		users = newMockUser("testUser");
		usersRepository.save(users);
		portfolios = newMockPortfolio(users);
		portfoliosRepository.save(portfolios);
	}

	@AfterEach
	void teardown() {
		portfoliosRepository.deleteAll();
	}

	@Test
	@DisplayName("좋아요 분산락 테스트")
	public void test_like_distribution_lock_10() throws InterruptedException {
		int numberOfThreads = 10;
		ExecutorService executorService = Executors.newFixedThreadPool(32);
		CountDownLatch latch = new CountDownLatch(numberOfThreads);

		for (int i = 0; i < numberOfThreads; i++) {
			executorService.submit(() -> {
				try {
					// 분산락 적용 메서드 호출
					portfolioLike.increaseLikeCount(String.valueOf(portfolios.getPortfolioId()),
						portfolios.getPortfolioId());
				} finally {
					latch.countDown();
				}
			});
		}
		latch.await();

		Portfolios persistPortfolio = portfoliosRepository.findById(portfolios.getPortfolioId())
			.orElseThrow(IllegalArgumentException::new);

		Assertions.assertThat(persistPortfolio.getLikeCount()).isEqualTo(numberOfThreads);
		System.out.println("좋아요 개수" + persistPortfolio.getLikeCount());
	}
	@Test
	@DisplayName("좋아요 분산락 테스트")
	public void test_like_distribution_lock_100() throws InterruptedException {
		int numberOfThreads = 100;
		ExecutorService executorService = Executors.newFixedThreadPool(32);
		CountDownLatch latch = new CountDownLatch(numberOfThreads);

		for (int i = 0; i < numberOfThreads; i++) {
			executorService.submit(() -> {
				try {
					// 분산락 적용 메서드 호출
					portfolioLike.increaseLikeCount(String.valueOf(portfolios.getPortfolioId()),
						portfolios.getPortfolioId());
				} finally {
					latch.countDown();
				}
			});
		}
		latch.await();

		Portfolios persistPortfolio = portfoliosRepository.findById(portfolios.getPortfolioId())
			.orElseThrow(IllegalArgumentException::new);

		Assertions.assertThat(persistPortfolio.getLikeCount()).isEqualTo(numberOfThreads);
		System.out.println("좋아요 개수" + persistPortfolio.getLikeCount());
	}
}