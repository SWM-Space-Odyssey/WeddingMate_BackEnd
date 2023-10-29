package swmaestro.spaceodyssey.weddingmate.domain.report.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ReportItemType {
	PORTFOLIO("포트폴리오"),
	ITEM("아이템"),
	USER("유저");

	private final String reportType;
}
