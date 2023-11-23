package swmaestro.spaceodyssey.weddingmate.domain.report.mapper;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportByReportedResDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportByReporterResDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.entity.Report;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ReportMapper {

	public ReportByReporterResDto entityToReportByReporterResDto(Report report) {
		return ReportByReporterResDto.builder()
				.reportedUserNickname(report.getReportedUser().getNickname())
				.reportReason(report.getReportReason())
				.reportItemType(report.getReportItemType())
				.reportTime(report.getReportTime())
				.build();
	}

	public ReportByReportedResDto entityToReportByReportedResDto(Report report) {
		return ReportByReportedResDto.builder()
				.reportReason(report.getReportReason())
				.reportItemType(report.getReportItemType())
				.reportTime(report.getReportTime())
				.build();
	}
}
