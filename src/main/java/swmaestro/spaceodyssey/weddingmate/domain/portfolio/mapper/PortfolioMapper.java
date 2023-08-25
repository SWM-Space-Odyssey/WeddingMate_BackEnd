package swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;

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

	public PortfolioDetailResDto entityToDto(Portfolio portfolio, List<ItemResDto> itemResDtoList, Boolean isWriter) {

		return PortfolioDetailResDto.builder()
			.title(portfolio.getTitle())
			.id(portfolio.getPortfolioId())
			.region(portfolio.getRegionTag())
			.tagList(portfolio.getPortfolioTagList())
			.itemResDtoList(itemResDtoList)
			.repImgUrl(portfolio.getFile().getUrl())
			.isWriter(isWriter)
			.plannerId(portfolio.getPlanner().getPlannerId())
			.build();
	}

	public Portfolio dtoToEntity(Planner planner, PortfolioSaveReqDto portfolioSaveReqDto) {

		return Portfolio.builder()
			.title(portfolioSaveReqDto.getTitle())
			.planner(planner)
			.regionTag(portfolioSaveReqDto.getRegion())
			.portfolioTagList(portfolioSaveReqDto.getTags())
			.build();
	}
}
