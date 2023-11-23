package swmaestro.spaceodyssey.weddingmate.domain.report.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.persistence.EntityManager;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.jpa.mapping.JpaMetamodelMappingContext;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.TestExecutionEvent;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportByReportedResDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportByReporterResDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.entity.Report;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportItemType;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportReason;
import swmaestro.spaceodyssey.weddingmate.domain.report.mapper.ReportMapper;
import swmaestro.spaceodyssey.weddingmate.domain.report.repository.ReportRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.REPORT_CREATE_SUCCESS;

@Slf4j
@ActiveProfiles("test")
@Sql("classpath:db/teardown.sql")
@AutoConfigureMockMvc
@MockBean(JpaMetamodelMappingContext.class) //JPA auditing 기능을 사용하기 때문에 JPA 메타 모델을 처리하기 위해 사용
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.MOCK)
public class ReportControllerTest extends DummyEntity {

	private final String BASE_URL = "/api/v1/report";
	/* =============== Entity =============== */
	Users reporterUser;
	Users reportedUser;
	Portfolios portfolio;
	Items item;
	/* =============== Repository =============== */
	@Autowired
	UsersRepository usersRepository;
	@Autowired
	PortfoliosRepository portfoliosRepository;
	@Autowired
	ItemsRepository itemsRepository;
	@Autowired
	ReportRepository reportRepository;
	@Autowired
	ReportMapper reportMapper;
	@Autowired
	private MockMvc mockMvc;
	@Autowired
	private ObjectMapper objectMapper;
	@Autowired
	private EntityManager entityManager;

	@BeforeEach
	void setup() {
		saveDummyData();
		entityManager.clear();
	}

	@ParameterizedTest
	@ValueSource(strings = {"ITEM", "PORTFOLIO", "USER"})
	@DisplayName("[성공] 신고 생성")
	@WithUserDetails(value = "reporter@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void makeReportWithDifferentReportItemTypes(String reportItemType) throws Exception {
		// given
		ReportReqDto reqDto = ReportReqDto.builder()
				.reportedUserId(reportedUser.getUserId())
				.reportedItemId(item.getItemId())
				.reportReason(ReportReason.FAKE)
				.reportItemType(ReportItemType.valueOf(reportItemType))
				.build();

		// when
		ResultActions resultActions = mockMvc.perform(
				MockMvcRequestBuilders.post(BASE_URL)
						.contentType(MediaType.APPLICATION_JSON)
						.content(objectMapper.writeValueAsString(reqDto))
		).andDo(print());

		// then
		resultActions.andExpect(MockMvcResultMatchers.status().is2xxSuccessful())
				.andExpect(jsonPath("$.status").value(ApiResponseStatus.SUCCESS.toString()))
				.andExpect(jsonPath("$.data").value(REPORT_CREATE_SUCCESS));
	}

	@Test
	@DisplayName("[성공] 자신이 신고한 내역 조회")
	@WithUserDetails(value = "reporter@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void getReportsByReporterUserIdTest() throws Exception {
		// given
		Report report1 = reportRepository.save(makeReport());
		Report report2 = reportRepository.save(makeReport());
		ReportByReporterResDto result1 = reportMapper.entityToReportByReporterResDto(report1);
		ReportByReporterResDto result2 = reportMapper.entityToReportByReporterResDto(report2);

		// when
		ResultActions resultActions = mockMvc.perform(
				MockMvcRequestBuilders.get(BASE_URL + "/by-reporter-user-id/{userId}", reporterUser.getUserId())
		).andDo(print());

		// then
		resultActions.andExpect(MockMvcResultMatchers.status().is2xxSuccessful())
				.andExpect(MockMvcResultMatchers.jsonPath("$.status").value(String.valueOf(ApiResponseStatus.SUCCESS)))
				.andExpect(jsonPath("$.data[0].reportedUserNickname").value(result1.getReportedUserNickname()))
				.andExpect(jsonPath("$.data[0].reportReason").value(result1.getReportReason().toString()))
				.andExpect(jsonPath("$.data[0].reportItemType").value(result1.getReportItemType().toString()))
				.andExpect(jsonPath("$.data[1].reportedUserNickname").value(result2.getReportedUserNickname()))
				.andExpect(jsonPath("$.data[1].reportReason").value(result2.getReportReason().toString()))
				.andExpect(jsonPath("$.data[1].reportItemType").value(result2.getReportItemType().toString()));
	}

	@Test
	@DisplayName("[성공] 자신이 신고당한 내역 조회")
	@WithUserDetails(value = "reported@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void getReportsByReportedUserIdTest() throws Exception {
		// given
		Report report1 = reportRepository.save(makeReport());
		Report report2 = reportRepository.save(makeReport());
		ReportByReportedResDto result1 = reportMapper.entityToReportByReportedResDto(report1);
		ReportByReportedResDto result2 = reportMapper.entityToReportByReportedResDto(report2);

		// when
		ResultActions resultActions = mockMvc.perform(
				MockMvcRequestBuilders.get(BASE_URL + "/by-reported-user-id/{userId}", reportedUser.getUserId())
		).andDo(print());

		// then
		resultActions.andExpect(MockMvcResultMatchers.status().is2xxSuccessful())
				.andExpect(jsonPath("$.data[0].reportReason").value(result1.getReportReason().toString()))
				.andExpect(jsonPath("$.data[0].reportItemType").value(result1.getReportItemType().toString()))
				.andExpect(jsonPath("$.data[1].reportReason").value(result2.getReportReason().toString()))
				.andExpect(jsonPath("$.data[1].reportItemType").value(result2.getReportItemType().toString()));
	}

	void saveDummyData() {
		reporterUser = usersRepository.save(newMockUser("reporter"));
		reportedUser = usersRepository.save(newMockUser("reported"));
		portfolio = portfoliosRepository.save(newMockPortfolio(reportedUser));
		item = itemsRepository.save(newMockItem(portfolio));
	}

	Report makeReport() {
		return Report.builder()
				.reporterUser(reporterUser)
				.reportedUser(reportedUser)
				.reportItemId(item.getItemId())
				.reportReason(ReportReason.SPAM)
				.reportItemType(ReportItemType.ITEM)
				.build();
	}
}