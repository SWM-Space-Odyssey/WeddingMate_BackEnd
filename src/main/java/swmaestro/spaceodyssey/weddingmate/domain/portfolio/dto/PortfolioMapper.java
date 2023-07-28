package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfolioMapper {

	public PortfolioListResDto entityToDto(Portfolio portfolio) {

		return PortfolioListResDto.builder()
			.portfolioId(portfolio.getPortfolioId())
			.title(portfolio.getTitle())
			.repImgUrl(portfolio.getFile().getUrl())
			.build();
	}
}
