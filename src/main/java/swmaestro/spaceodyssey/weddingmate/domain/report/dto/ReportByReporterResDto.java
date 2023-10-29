package swmaestro.spaceodyssey.weddingmate.domain.report.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportItemType;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportReason;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ReportByReporterResDto {
	private String reportedUserNickname;
	private ReportReason reportReason;
	private ReportItemType reportItemType;
	private LocalDateTime reportTime;

	@Builder
	public ReportByReporterResDto(String reportedUserNickname, ReportReason reportReason,
								  ReportItemType reportItemType, LocalDateTime reportTime) {
		this.reportedUserNickname = reportedUserNickname;
		this.reportReason = reportReason;
		this.reportItemType = reportItemType;
		this.reportTime = reportTime;
	}
}
