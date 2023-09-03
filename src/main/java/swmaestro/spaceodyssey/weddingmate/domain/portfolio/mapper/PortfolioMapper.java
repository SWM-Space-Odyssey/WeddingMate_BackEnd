package swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikeRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfolioMapper {
	private final LikeRepository likeRepository;

	public PortfolioListResDto entityToDto(Users users, Portfolio portfolio) {

		Boolean isPortfolioLiked = isPortfolioLikedByUser(users, portfolio.getPortfolioId());

		return PortfolioListResDto.builder()
			.portfolioId(portfolio.getPortfolioId())
			.title(portfolio.getTitle())
			.repImgUrl(portfolio.getFile().getUrl())
			.isLiked(isPortfolioLiked)
			.build();
	}

	public PortfolioDetailResDto entityToDto(Portfolio portfolio, List<ItemResDto> itemResDtoList, Boolean isWriter, Users users) {

		Boolean isPortfolioLiked = isPortfolioLikedByUser(users, portfolio.getPortfolioId());

		return PortfolioDetailResDto.builder()
			.title(portfolio.getTitle())
			.id(portfolio.getPortfolioId())
			.region(portfolio.getRegionTag())
			.tagList(portfolio.getPortfolioTagList())
			.itemResDtoList(itemResDtoList)
			.repImgUrl(portfolio.getFile().getUrl())
			.isWriter(isWriter)
			.plannerId(portfolio.getPlanner().getPlannerId())
			.isLiked(isPortfolioLiked)
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

	private boolean isPortfolioLikedByUser(Users users, Long portfolioId) {
		return !likeRepository.findByUsersAndLikeTypeAndLikedId(users, LikeEnum.PORTFOLIO, portfolioId).isEmpty();
	}
}
