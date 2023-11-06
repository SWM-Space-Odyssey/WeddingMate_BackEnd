package swmaestro.spaceodyssey.weddingmate.domain.report.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportItemType;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportReason;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ReportReqDto {
	private Long reportedUserId;
	private Long reportedItemId;
	private ReportReason reportReason;
	private ReportItemType reportItemType;

	@Builder
	public ReportReqDto(Long reportedUserId, Long reportedItemId,
						ReportReason reportReason, ReportItemType reportItemType) {
		this.reportedUserId = reportedUserId;
		this.reportedItemId = reportedItemId;
		this.reportReason = reportReason;
		this.reportItemType = reportItemType;
	}
}
