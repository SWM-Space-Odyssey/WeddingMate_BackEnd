package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class PortfolioListResDto {

	private Long portfolioId;
	private String title;

	@Builder
	public PortfolioListResDto(Long portfolioId, String title) {
		this.portfolioId = portfolioId;
		this.title = title;
	}

}
