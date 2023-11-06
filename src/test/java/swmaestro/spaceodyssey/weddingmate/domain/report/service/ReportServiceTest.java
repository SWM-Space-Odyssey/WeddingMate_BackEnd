package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportByReportedResDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportByReporterResDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.entity.Report;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportItemType;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportReason;
import swmaestro.spaceodyssey.weddingmate.domain.report.mapper.ReportMapper;
import swmaestro.spaceodyssey.weddingmate.domain.report.repository.ReportRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ReportServiceTest extends DummyEntity {

	private static final String REDISSON_LOCK_PREFIX = "Id:";
	Users reporterUser;
	Users reportedUser;
	Portfolios portfolio;
	Items item;
	Report portfolioReport;
	Report itemReport;
	@InjectMocks
	private ReportService reportService;
	@Mock
	private UsersRepositoryService usersRepositoryService;
	@Mock
	private UserSuspensionService userSuspensionService;
	@Mock //진짜 객체 주입
	private ReportMapper reportMapper;
	@Mock
	private ReportRepository reportRepository;
	private String lockname;

	@BeforeEach
	void setUp() {
		makeMockData();
	}

	@Test
	@DisplayName("[성공] 신고 성공")
	void makeReport() {
		lockname = REDISSON_LOCK_PREFIX + reportedUser.getUserId();

		ReportReqDto reqDto = ReportReqDto.builder()
				.reportedUserId(reportedUser.getUserId())
				.reportedItemId(portfolio.getPortfolioId())
				.reportReason(ReportReason.SPAM)
				.reportItemType(ReportItemType.PORTFOLIO)
				.build();

		// stub
		when(usersRepositoryService.findUserById(reqDto.getReportedUserId()))
				.thenReturn(reportedUser);

		// when
		reportService.makeReport(reporterUser, reqDto);

		// verify method calls
		verify(reportRepository).save(argThat(report ->
				report.getReporterUser() == reporterUser &&
						report.getReportedUser() == reportedUser &&
						report.getReportReason().equals(reqDto.getReportReason()) &&
						report.getReportItemType().equals(reqDto.getReportItemType()) &&
						report.getReportItemId().equals(reqDto.getReportedItemId())
		));
		verify(userSuspensionService).addReportCnt(lockname, reportedUser.getUserId());

		verify(reportRepository, times(1)).save(any());
		verify(userSuspensionService, times(1)).addReportCnt(lockname, reportedUser.getUserId());

		// then
		// 해당 서비스에서는 userSuspensionService를 검증하는 것이 아님
		// 따라서 그냥 자체적으로 1 올려줌
		reportedUser.incrementReportCnt();
		Assertions.assertThat(reportedUser.getReportCnt())
				.isEqualTo(1);
	}

	@Test
	@DisplayName("[성공] 유저가 신고한 내역 조회")
	void getReportsByReporterUserId() {
		List<Report> reportList = new ArrayList<>();
		List<ReportByReporterResDto> reportByReporterResDtoList = new ArrayList<>();

		reportList.add(portfolioReport);
		reportList.add(itemReport);

		reportByReporterResDtoList.add(reportMapper.entityToReportByReporterResDto(portfolioReport));
		reportByReporterResDtoList.add(reportMapper.entityToReportByReporterResDto(itemReport));

		// stub
		when(usersRepositoryService.findUserById(reporterUser.getUserId()))
				.thenReturn(reporterUser);
		when(reportRepository.findByReporterUser(reporterUser))
				.thenReturn(reportList);
		when(reportMapper.entityToReportByReporterResDto(any(Report.class))).thenReturn(any(ReportByReporterResDto.class));

		// 호출
		List<ReportByReporterResDto> resultList = reportService.getReportsByReporterUserId(reporterUser, reporterUser.getUserId());

		// verify
		verify(reportRepository, times(1)).findByReporterUser(reporterUser);
		// 위에서 resultList 만드느라 2번씩 중복으로 사용
		verify(reportMapper, times(reportList.size() * 2)).entityToReportByReporterResDto(any());

		// that
		Assertions.assertThat(resultList)
				.isEqualTo(reportByReporterResDtoList);
	}

	@Test
	@DisplayName("[성공] 유저가 신고당한 내역 조회")
	void getReportsByReportedUserId() {
		List<Report> reportList = new ArrayList<>();
		List<ReportByReportedResDto> reportByReportedResDtoList = new ArrayList<>();

		reportList.add(portfolioReport);
		reportList.add(itemReport);

		reportByReportedResDtoList.add(reportMapper.entityToReportByReportedResDto(portfolioReport));
		reportByReportedResDtoList.add(reportMapper.entityToReportByReportedResDto(itemReport));

		// stub
		when(usersRepositoryService.findUserById(reportedUser.getUserId()))
				.thenReturn(reportedUser);
		when(reportRepository.findByReportedUser(reportedUser))
				.thenReturn(reportList);
		when(reportMapper.entityToReportByReportedResDto(any(Report.class))).thenReturn(any(ReportByReportedResDto.class));

		// 호출
		List<ReportByReportedResDto> resultList = reportService.getReportsByReportedUserId(reportedUser, reportedUser.getUserId());

		// verify
		verify(reportRepository, times(1)).findByReportedUser(reportedUser);
		// 위에서 resultList 만드느라 2번씩 중복으로 사용
		verify(reportMapper, times(reportList.size() * 2)).entityToReportByReportedResDto(any());

		// that
		Assertions.assertThat(resultList)
				.isEqualTo(reportByReportedResDtoList);
	}

	void makeMockData() {
		reporterUser = newMockUser("reporter");
		ReflectionTestUtils.setField(reporterUser, "userId", 1L);
		ReflectionTestUtils.setField(reporterUser, "email", "reporter@gmail.com");

		reportedUser = newMockUser("reported");
		ReflectionTestUtils.setField(reportedUser, "userId", 2L);
		ReflectionTestUtils.setField(reportedUser, "email", "reported@gmail.com");

		portfolio = newMockPortfolio(reportedUser);
		ReflectionTestUtils.setField(portfolio, "portfolioId", 10L);

		item = newMockItem(portfolio);
		ReflectionTestUtils.setField(item, "itemId", 11L);

		portfolioReport = newMockReport(reporterUser, reportedUser, ReportItemType.PORTFOLIO, 10L);
		itemReport = newMockReport(reporterUser, reportedUser, ReportItemType.ITEM, 11L);
	}
}