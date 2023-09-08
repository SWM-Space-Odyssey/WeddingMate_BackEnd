package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfolioSaveReqDto {
	private String title;
	private String tags;
	private String region;

	@Builder
	public PortfolioSaveReqDto(String title, String tags, String region) {
		this.title = title;
		this.tags = tags;
		this.region = region;
	}
}
