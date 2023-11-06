package swmaestro.spaceodyssey.weddingmate.domain.company.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class CompanyItemResDto {
	private String repImgUrl;
	private String itemRecord;
	private String itemDate;
	private Long userId;

	@Builder
	public CompanyItemResDto(String repImgUrl, String itemRecord, String itemDate, Long userId) {
		this.repImgUrl = repImgUrl;
		this.itemRecord = itemRecord;
		this.itemDate = itemDate;
		this.userId = userId;
	}
}
