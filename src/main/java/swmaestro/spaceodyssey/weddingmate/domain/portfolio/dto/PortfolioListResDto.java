package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class PortfolioListResDto {

	private Long portfolioId;
	private String title;
	private String repImgUrl;

	@Builder
	public PortfolioListResDto(Long portfolioId, String title, String repImgUrl) {
		this.portfolioId = portfolioId;
		this.title = title;
		this.repImgUrl = repImgUrl;
	}

}
