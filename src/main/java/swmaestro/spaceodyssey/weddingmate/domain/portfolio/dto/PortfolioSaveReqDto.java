package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class PortfolioSaveReqDto {
	private String title;
	private String tags;
	private String region;
}
