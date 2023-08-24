package swmaestro.spaceodyssey.weddingmate.domain.like.repository;

import java.util.Arrays;
import java.util.Collection;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;

import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannerRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;

@ActiveProfiles("test")
@Sql("classpath:db/teardown.sql")
@AutoConfigureMockMvc
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.MOCK)
public class LikeRepositoryTest extends DummyEntity {
	@Autowired
	UsersRepository usersRepository;

	@Autowired
	ItemRepository itemRepository;

	@Autowired
	PortfolioRepository portfolioRepository;

	@Autowired
	LikeRepository likeRepository;

	@Autowired
	PlannerRepository plannerRepository;


	Integer count = 5000;

	@BeforeEach
	void setup() {
		dataSetting();
	}

	@Test
	public void testLikeRepositoryFindByUsersAndLikeTypeAndLikedId() {
		Users users = usersRepository.findByNickname("test0").get();

		long startTime = System.currentTimeMillis();
		likeRepository.findByUsersAndLikeTypeAndLikedId(users,LikeEnum.ITEM, Long.valueOf(1));
		long endTime = System.currentTimeMillis();
		long executionTime = endTime - startTime;

		System.out.println("첫번째 Execution time: " + executionTime + " milliseconds");
	}

	@Test
	public void testLikeRepositoryFindByUsersAndLikeTypeIn() {
		Users users = usersRepository.findByNickname("test0").get();
		Collection<LikeEnum> likeTypes = Arrays.asList(LikeEnum.PORTFOLIO, LikeEnum.ITEM);

		long startTime = System.currentTimeMillis();
		likeRepository.findByUsersAndLikeTypeIn(users, likeTypes);
		long endTime = System.currentTimeMillis();
		long executionTime = endTime - startTime;

		System.out.println("Execution time: " + executionTime + " milliseconds");
	}

	private void dataSetting() {
		for (int i = 0; i < count; i++) {
			Users users = usersRepository.save(newMockUser("test" + i));
			likeRepository.save(mockUserLike((long)i, LikeEnum.PORTFOLIO, users));
			likeRepository.save(mockUserLike((long)i, LikeEnum.PLANNER, users));
			likeRepository.save(mockUserLike((long)i, LikeEnum.ITEM, users));
		}
	}
}
