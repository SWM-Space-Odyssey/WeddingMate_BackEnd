package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportByReportedResDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportByReporterResDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.entity.Report;
import swmaestro.spaceodyssey.weddingmate.domain.report.mapper.ReportMapper;
import swmaestro.spaceodyssey.weddingmate.domain.report.repository.ReportRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ReportService {

	private static final String REDISSON_LOCK_PREFIX = "Id:";
	private final ReportRepository reportRepository;
	private final UsersRepositoryService usersRepositoryService;
	private final UserSuspensionService userSuspensionService;
	private final ReportMapper reportMapper;

	@Transactional
	public void makeReport(Users user, ReportReqDto reqDto) {
		Users reportedUser = usersRepositoryService.findUserById(reqDto.getReportedUserId());
		Report report = Report.builder()
				.reporterUser(user)
				.reportedUser(reportedUser)
				.reportReason(reqDto.getReportReason())
				.reportItemType(reqDto.getReportItemType())
				.reportItemId(reqDto.getReportedItemId())
				.build();
		reportRepository.save(report);

		String lockName = REDISSON_LOCK_PREFIX + reportedUser.getUserId();
		userSuspensionService.addReportCnt(lockName, reportedUser.getUserId());
	}

	@Transactional(readOnly = true)
	public List<ReportByReporterResDto> getReportsByReporterUserId(Users user, Long userId) {
		verifyCurrentUserMatchesTargetUser(user, userId);

		List<Report> reportListByReporter = reportRepository.findByReporterUser(user);
		return reportListByReporter.stream()
				.map(reportMapper::entityToReportByReporterResDto)
				.toList();
	}

	@Transactional(readOnly = true)
	public List<ReportByReportedResDto> getReportsByReportedUserId(Users user, Long userId) {
		verifyCurrentUserMatchesTargetUser(user, userId);

		List<Report> reportListByReported = reportRepository.findByReportedUser(user);
		return reportListByReported.stream()
				.map(reportMapper::entityToReportByReportedResDto)
				.toList();
	}

	private void verifyCurrentUserMatchesTargetUser(Users currentUser, Long targetUserId) {
		Users targetUser = usersRepositoryService.findUserById(targetUserId);
		if (!currentUser.getEmail().equals(targetUser.getEmail())) {
			throw new UserUnAuthorizedException();
		}
	}
}
