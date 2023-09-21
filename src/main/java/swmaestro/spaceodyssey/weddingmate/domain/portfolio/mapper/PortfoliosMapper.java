package swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfoliosMapper {
	private final LikesRepository likeRepository;

	public PortfolioListResDto entityToDto(Users users, Portfolios portfolios) {

		Boolean isPortfolioLiked = isPortfolioLikedByUser(users, portfolios.getPortfolioId());

		return PortfolioListResDto.builder()
			.portfolioId(portfolios.getPortfolioId())
			.title(portfolios.getTitle())
			.repImgUrl(portfolios.getFiles().getUrl())
			.isLiked(isPortfolioLiked)
			.build();
	}

	public PortfolioDetailResDto entityToDto(Portfolios portfolios, List<ItemResDto> itemResDtoList, Boolean isWriter, Users users) {

		Boolean isPortfolioLiked = isPortfolioLikedByUser(users, portfolios.getPortfolioId());

		return PortfolioDetailResDto.builder()
			.title(portfolios.getTitle())
			.id(portfolios.getPortfolioId())
			.region(portfolios.getRegionTag())
			.tagList(portfolios.getPortfolioTagList())
			.itemResDtoList(itemResDtoList)
			.repImgUrl(portfolios.getFiles().getUrl())
			.isWriter(isWriter)
			.userId(portfolios.getUsers().getUserId())
			.isLiked(isPortfolioLiked)
			.build();
	}

	public Portfolios dtoToEntity(Users users, PortfolioSaveReqDto portfolioSaveReqDto) {

		return Portfolios.builder()
			.title(portfolioSaveReqDto.getTitle())
			.users(users)
			.regionTag(portfolioSaveReqDto.getRegion())
			.portfolioTagList(portfolioSaveReqDto.getTags())
			.build();
	}

	private boolean isPortfolioLikedByUser(Users users, Long portfolioId) {
		return !likeRepository.findByUsersAndLikeTypeAndLikedId(users, LikeEnum.PORTFOLIO, portfolioId).isEmpty();
	}
}
