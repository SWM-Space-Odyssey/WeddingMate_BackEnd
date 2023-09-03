package swmaestro.spaceodyssey.weddingmate.domain.portfolio.controller;

import static org.hamcrest.Matchers.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import java.io.FileInputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.test.context.support.TestExecutionEvent;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import jakarta.persistence.EntityManager;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemOrderDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannerRepository;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.service.PortfolioService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;

@ActiveProfiles("test")
@Sql("classpath:db/teardown.sql")
@AutoConfigureMockMvc
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.MOCK)
public class PortfolioControllerTest extends DummyEntity {

	@Autowired
	private PortfolioService portfolioService;

	@Autowired
	private MockMvc mockMvc;

	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private UsersRepository usersRepository;

	@Autowired
	private PortfolioRepository portfolioRepository;

	@Autowired
	private PlannerRepository plannerRepository;

	@Autowired
	private ItemRepository itemRepository;

	@Autowired
	private EntityManager entityManager;

	Portfolio testPortfolio1;

	@BeforeEach
	public void setUp() {
		dataSetting();
		entityManager.clear();
		System.out.println(testPortfolio1);
	}

	@Test
	@DisplayName("포트폴리오 생성 성공")
	@WithUserDetails(value = "test@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void createPortfolio() throws Exception {
		//given1
		final String filePath = "src/test/java/swmaestro/spaceodyssey/weddingmate/domain/portfolio/test.jpg"; //파일경로
		FileInputStream fileInputStream = new FileInputStream(filePath);
		MockMultipartFile multipartFile = new MockMultipartFile("file", "test.png", "image/png", fileInputStream);

		//given2
		PortfolioSaveReqDto portfolioSaveReqDto = PortfolioSaveReqDto.builder()
			.title("test")
			.tags("tag1, tag2")
			.region("서울")
			.build();
		String req = objectMapper.writeValueAsString(portfolioSaveReqDto);
		MockMultipartFile request = new MockMultipartFile("portfolioSaveReqDto", "req", "application/json",
			req.getBytes(StandardCharsets.UTF_8));

		//when
		ResultActions resultActions = mockMvc.perform(
			multipart("/api/v1/portfolio/save")
				.file(multipartFile)
				.file(request)
		);

		//then
		resultActions.andExpect(status().is2xxSuccessful());
	}

	@Test
	@DisplayName("포트폴리오 생성 실패")
	@WithUserDetails(value = "test@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void createPortfolioFailNoFile() throws Exception {
		//given
		PortfolioSaveReqDto portfolioSaveReqDto = PortfolioSaveReqDto.builder()
			.title("test")
			.tags("tag1, tag2")
			.region("서울")
			.build();
		String req = objectMapper.writeValueAsString(portfolioSaveReqDto);
		MockMultipartFile request = new MockMultipartFile("portfolioSaveReqDto", "req", "application/json",
			req.getBytes(StandardCharsets.UTF_8));

		//when
		ResultActions resultActions = mockMvc.perform(
			multipart("/api/v1/portfolio/save")
				.file(request)
		);

		//then
		resultActions.andExpect(status().is4xxClientError());
	}

	@Test
	@DisplayName("포트폴리오 조회 성공")
	@WithUserDetails(value = "test@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void getPortfolioByUser() throws Exception {
		//when
		ResultActions resultActions = mockMvc.perform(
			MockMvcRequestBuilders.get("/api/v1/portfolio/")
		);

		//then
		resultActions.andExpect(status().is2xxSuccessful())
			.andExpect(jsonPath("$").isArray())
			.andExpect(jsonPath("$", hasSize(1)))
			.andExpect(jsonPath("$[0].title").value("Test Title"))
			.andExpect(jsonPath("$[0].repImgUrl").value("/test"));
	}

	@Test
	@DisplayName("포트폴리오 조회 성공")
	@WithUserDetails(value = "test@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void getPortfolioById() throws Exception {
		//given
		Long portfolioId = testPortfolio1.getPortfolioId();

		//when
		ResultActions resultActions = mockMvc.perform(
			MockMvcRequestBuilders.get("/api/v1/portfolio/{id}", portfolioId)
		).andDo(print());

		//then
		resultActions.andExpect(status().is2xxSuccessful());
	}

	@Test
	@DisplayName("포트폴리오 조회 실패")
	@WithUserDetails(value = "test@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void getPortfolioIsDeleted() throws Exception {
		//given
		testPortfolio1.deletePortfolio();
		portfolioRepository.save(testPortfolio1);

		//when
		ResultActions resultActions = mockMvc.perform(
			MockMvcRequestBuilders.get("/api/v1/portfolio/{id}", testPortfolio1.getPortfolioId())
		).andDo(print());

		//then
		resultActions.andExpect(status().is4xxClientError());
	}

	@Test
	@DisplayName("포트폴리오 업데이트 성공")
	@WithUserDetails(value = "test@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void updatePortfolioById() throws Exception {
		Long portfolioId = testPortfolio1.getPortfolioId();

		//given1
		final String filePath = "src/test/java/swmaestro/spaceodyssey/weddingmate/domain/portfolio/test.jpg"; //파일경로
		FileInputStream fileInputStream = new FileInputStream(filePath);
		MockMultipartFile multipartFile = new MockMultipartFile("file", "test.png", "image/png", fileInputStream);

		//given2
		Item item1 = itemRepository.save(newMockItem(testPortfolio1));
		Item item2 = itemRepository.save(newMockItem(testPortfolio1));
		ItemOrderDto itemOrderDto = new ItemOrderDto(item1.getItemId(), 2);
		ItemOrderDto itemOrderDto1 = new ItemOrderDto(item2.getItemId(), 1);
		List<ItemOrderDto> itemOrderList = new ArrayList<>();
		itemOrderList.add(itemOrderDto);
		itemOrderList.add(itemOrderDto1);

		//given3
		PortfolioUpdateReqDto portfolioUpdateReqDto = PortfolioUpdateReqDto.builder()
			.title("아름다운 결혼식")
			.region("서울")
			.tags("고급스러운")
			.itemOrderList(itemOrderList)
			.build();
		String req = objectMapper.writeValueAsString(portfolioUpdateReqDto);
		MockMultipartFile request = new MockMultipartFile("portfolioUpdateReqDto", "req", "application/json",
			req.getBytes(StandardCharsets.UTF_8));

		//when
		ResultActions resultActions = mockMvc.perform(
			multipart("/api/v1/portfolio/{id}", portfolioId)
				.file(multipartFile)
				.file(request)
		).andDo(print());

		//then
		resultActions.andExpect(status().is2xxSuccessful());
	}

	@Test
	@DisplayName("포트폴리오 삭제")
	@WithUserDetails(value = "test@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void deletePortfolioById() throws Exception {
		//given
		Item item1 = itemRepository.save(newMockItem(testPortfolio1));
		Item item2 = itemRepository.save(newMockItem(testPortfolio1));

		//when
		ResultActions resultActions = mockMvc.perform(
			MockMvcRequestBuilders
				.delete("/api/v1/portfolio/" + testPortfolio1.getPortfolioId())

		);

		//then
		resultActions.andExpect(status().is2xxSuccessful());
	}

	void dataSetting() {
		Users test = usersRepository.save(newMockUser("test"));
		Planner planner = plannerRepository.save(newMockPlanner(test));
		testPortfolio1 = portfolioRepository.save(newMockPortfolio(planner));
	}
}
