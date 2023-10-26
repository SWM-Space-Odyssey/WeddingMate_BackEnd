package swmaestro.spaceodyssey.weddingmate.domain.report.entity;

import jakarta.persistence.*;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportItemType;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportReason;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

import java.time.LocalDateTime;

@NoArgsConstructor
@Getter
@Entity
public class Report extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(updatable = false)
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "reporter_id", nullable = false)
	private Users reporterUser;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "reported_id", nullable = false)
	private Users reportedUser;

	@Column(nullable = false)
	private ReportReason reportReason;

	@Column(nullable = false)
	private ReportItemType reportItemType;

	@Column(nullable = false)
	private Long reportItemId;

	private LocalDateTime reportTime;

	@Builder
	public Report(Users reporterUser, Users reportedUser,
				  ReportReason reportReason, ReportItemType reportItemType,
				  Long reportItemId) {
		this.reporterUser = reporterUser;
		this.reportedUser = reportedUser;
		this.reportReason = reportReason;
		this.reportItemType = reportItemType;
		this.reportItemId = reportItemId;
		this.reportTime = LocalDateTime.now();
	}
}
