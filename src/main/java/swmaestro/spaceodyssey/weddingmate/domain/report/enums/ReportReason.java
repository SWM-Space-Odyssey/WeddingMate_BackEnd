package swmaestro.spaceodyssey.weddingmate.domain.report.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ReportReason {

	SPAM("스팸"),
	SEXUAL("나체 이미지 또는 성적 행위"),
	ILLEGAL("불법적인 정보"),
	FAKE("거짓 정보"),
	INTELLECTUAL_PROPERTY("지식재산권 침해"),
	OTHER("기타");

	private final String name;
}
