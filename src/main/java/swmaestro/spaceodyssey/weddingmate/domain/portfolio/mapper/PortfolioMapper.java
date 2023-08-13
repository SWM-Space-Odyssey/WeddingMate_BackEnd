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
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

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

	public PortfolioDetailResDto entityToDto(Portfolio portfolio, List<ItemResDto> itemResDtoList) {

		return PortfolioDetailResDto.builder()
			.title(portfolio.getTitle())
			.id(portfolio.getPortfolioId())
			.tagList(portfolio.getPortfolioTagList())
			.itemResDtoList(itemResDtoList)
			.repImgUrl(portfolio.getFile().getUrl())
			.build();
	}

	public Portfolio dtoToEntity(Users users, PortfolioSaveReqDto portfolioSaveReqDto) {

		return Portfolio.builder()
			.title(portfolioSaveReqDto.getTitle())
			.users(users)
			.regionTag(portfolioSaveReqDto.getRegion())
			.portfolioTagList(portfolioSaveReqDto.getTags())
			.build();
	}
}
