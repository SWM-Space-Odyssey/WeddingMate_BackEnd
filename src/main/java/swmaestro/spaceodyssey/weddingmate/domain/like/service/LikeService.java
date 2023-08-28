package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PlannerLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PortfolioLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLike;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikeMapper;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikeRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannerRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerNotFoundException;

@Transactional
@Service
@RequiredArgsConstructor
public class LikeService {

	private final LikeMapper likeMapper;
	private final LikeRepository likeRepository;
	private final PlannerRepository plannerRepository;
	private final PortfolioRepository portfolioRepository;
	private final ItemRepository itemRepository;


	public boolean like(Long id, Users users, LikeEnum likeEnum) {
		List<UserLike> likeList = getLikesByUsersAndTypeAndId(users, likeEnum, id);

		if (likeList.isEmpty()) {
			saveLike(users, id, likeEnum);
			updateLikeCount(id, likeEnum, true);
			return true;
		}

		deleteLike(users, id, likeEnum);
		updateLikeCount(id, likeEnum, false);
		return false;
	}


	/*================== User가 좋아요 한 Entity 리스트 반환 ==================*/

	public List<PortfolioLikeResDto> getUserLikedPortfolio(Users users) {
		List<UserLike> likeList = getLikesByUsersAndType(users, LikeEnum.PORTFOLIO);

		return likeList.stream()
			.map(userLike -> findPortfolioById(userLike.getLikedId()))
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(likeMapper::entityToDto)
			.toList();
	}

	public List<PortfolioLikeResDto> getUserLikedItem(Users users) {
		List<UserLike> likeList = getLikesByUsersAndType(users, LikeEnum.ITEM);

		return likeList.stream()
			.map(userLike -> findItemById(userLike.getLikedId()))
			.filter(item -> (Boolean.FALSE.equals(item.getIsDeleted())))
			.map(likeMapper::entityToDto)
			.toList();
	}

	public List<PlannerLikeResDto> getUserLikedPlanner(Users users) {

		List<UserLike> likeList = getLikesByUsersAndType(users, LikeEnum.PLANNER);

		return likeList.stream()
			.map(userLike -> findPlannerById(userLike.getLikedId()))
			.map(likeMapper::entityToDto)
			.toList();
	}

	/*================== LikeCount 업데이트 ==================*/
	private void updateLikeCount(Long id, LikeEnum likeEnum, boolean isIncrement) {

		switch (likeEnum) {
			case PORTFOLIO:
				updatePortfolioLikeCount(id, isIncrement);
				break;
			case ITEM:
				updateItemLikeCount(id, isIncrement);
				break;
			case PLANNER:
				updatePlannerLikeCount(id, isIncrement);
				break;
		}
	}

	private void updatePortfolioLikeCount(Long id, boolean isIncrement) {

		Portfolio portfolio = findPortfolioById(id);

		if (portfolio != null) {
			if (isIncrement) {
				portfolio.setLikeCount(portfolio.getLikeCount() + 1);
			} else {
				portfolio.setLikeCount(portfolio.getLikeCount() - 1);
			}
			portfolioRepository.save(portfolio);
		}
	}

	private void updateItemLikeCount(Long id, boolean isIncrement) {

		Item item = findItemById(id);

		if (item != null) {
			if (isIncrement) {
				item.setLikeCount(item.getLikeCount() + 1);
			} else {
				item.setLikeCount(item.getLikeCount() - 1);
			}
			itemRepository.save(item);
		}
	}

	private void updatePlannerLikeCount(Long id, boolean isIncrement) {

		Planner planner = findPlannerById(id);

		if (planner != null) {
			if (isIncrement) {
				planner.setLikeCount(planner.getLikeCount() + 1);
			} else {
				planner.setLikeCount(planner.getLikeCount() - 1);
			}
			plannerRepository.save(planner);
		}
	}

	/*================== Repository 접근 ==================*/
	private List<UserLike> getLikesByUsersAndTypeAndId(Users users, LikeEnum likeEnum, Long id) {
		return likeRepository.findByUsersAndLikeTypeAndLikedId(users, likeEnum, id);
	}

	public List<UserLike> getLikesByUsersAndType(Users users, LikeEnum likeType) {
		return likeRepository.findByUsersAndLikeType(users, likeType);
	}

	private void saveLike(Users users, Long id, LikeEnum likeEnum) {
		UserLike like = UserLike.builder().users(users).likedId(id).likeEnum(likeEnum).build();
		likeRepository.save(like);
	}

	private void deleteLike(Users users, Long id, LikeEnum likeEnum) {
		likeRepository.deleteByUsersAndLikeTypeAndLikedId(users, likeEnum, id);
	}

	public Planner findPlannerById(Long id) {
		return plannerRepository.findById(id)
			.orElseThrow(PlannerNotFoundException::new);
	}

	public Portfolio findPortfolioById(Long id) {
		return portfolioRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);
	}

	public Item findItemById(Long id) {
		return itemRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);
	}
}

