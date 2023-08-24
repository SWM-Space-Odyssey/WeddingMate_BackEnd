package swmaestro.spaceodyssey.weddingmate.domain.like.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PlannerLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PortfolioLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLike;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class LikeMapper {

	public final PortfolioRepository portfolioRepository;
	public final ItemRepository itemRepository;

	public PortfolioLikeResDto entityToDto(Portfolio portfolio) {

		return PortfolioLikeResDto.builder()
			.id(portfolio.getPortfolioId())
			.title(portfolio.getTitle())
			.repImgUrl(portfolio.getFile().getUrl())
			.build();
	}

	public PortfolioLikeResDto entityToDto(Item item) {

		return PortfolioLikeResDto.builder()
			.id(item.getItemId())
			.title(item.getItemRecord())
			.repImgUrl(item.getFileList().get(0).getUrl())
			.build();
	}

	public PlannerLikeResDto entityToDto(Planner planner) {

		return PlannerLikeResDto.builder()
			.id(planner.getPlannerId())
			.name(planner.getUsers().getNickname())
			.profileImg(planner.getUsers().getProfileImage().getUrl())
			.build();
	}

	public PortfolioLikeResDto entityToDto(UserLike like) {
		if (like.getLikeType().equals(LikeEnum.PORTFOLIO)) {
			Portfolio portfolio = portfolioRepository.findById(like.getLikedId())
				.orElseThrow(PortfolioNotFoundException::new);
			return entityToDto(portfolio);
		} else {
			Item item = itemRepository.findById(like.getLikedId())
				.orElseThrow(ItemNotFoundException::new);
			return entityToDto(item);
		}
	}
}
