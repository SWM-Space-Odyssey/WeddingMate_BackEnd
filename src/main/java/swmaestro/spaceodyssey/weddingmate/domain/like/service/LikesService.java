package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.company.repository.CompaniesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.CompanyLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PlannerLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PortfolioLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.company.CompanyNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerNotFoundException;

@Transactional
@Service
@RequiredArgsConstructor
public class LikesService {

	private final LikesMapper likesMapper;
	private final LikesRepository likeRepository;
	private final PlannersRepository plannersRepository;
	private final PortfoliosRepository portfoliosRepository;
	private final ItemsRepository itemsRepository;
	private final CompaniesRepository companiesRepository;


	public boolean like(Long id, Users users, LikeEnum likeEnum) {
		List<UserLikes> likeList = getLikesByUsersAndTypeAndId(users, likeEnum, id);

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
		List<UserLikes> likeList = getLikesByUsersAndType(users, LikeEnum.PORTFOLIO);

		return likeList.stream()
			.map(userLikes -> findPortfolioById(userLikes.getLikedId()))
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(likesMapper::entityToDto)
			.toList();
	}

	public List<PortfolioLikeResDto> getUserLikedItem(Users users) {
		List<UserLikes> likeList = getLikesByUsersAndType(users, LikeEnum.ITEM);

		return likeList.stream()
			.map(userLikes -> findItemById(userLikes.getLikedId()))
			.filter(item -> (Boolean.FALSE.equals(item.getIsDeleted())))
			.map(likesMapper::entityToDto)
			.toList();
	}

	public List<PlannerLikeResDto> getUserLikedPlanner(Users users) {

		List<UserLikes> likeList = getLikesByUsersAndType(users, LikeEnum.PLANNER);

		return likeList.stream()
			.map(userLikes -> findPlannerById(userLikes.getLikedId()))
			.map(likesMapper::entityToDto)
			.toList();
	}

	public List<CompanyLikeResDto> getUserLikedCompany(Users users) {

		List<UserLikes> likeList = getLikesByUsersAndType(users, LikeEnum.COMPANY);

		return likeList.stream()
			.map(userLikes -> findCompanyById(userLikes.getLikedId()))
			.map(likesMapper::entityToDto)
			.toList();
	}

	/*================== LikeCount 업데이트 ==================*/
	private void updateLikeCount(Long id, LikeEnum likeEnum, boolean isIncrement) {

		switch (likeEnum) {
			case PORTFOLIO -> updatePortfolioLikeCount(id, isIncrement);
			case ITEM -> updateItemLikeCount(id, isIncrement);
			case PLANNER -> updatePlannerLikeCount(id, isIncrement);
			case COMPANY -> updateCompanyLikeCount(id, isIncrement);
		}
	}

	private void updatePortfolioLikeCount(Long id, boolean isIncrement) {

		Portfolios portfolios = findPortfolioById(id);

		if (portfolios != null) {
			if (isIncrement) {
				portfolios.setLikeCount(portfolios.getLikeCount() + 1);
			} else {
				portfolios.setLikeCount(portfolios.getLikeCount() - 1);
			}
			portfoliosRepository.save(portfolios);
		}
	}

	private void updateItemLikeCount(Long id, boolean isIncrement) {

		Items items = findItemById(id);

		if (items != null) {
			if (isIncrement) {
				items.setLikeCount(items.getLikeCount() + 1);
			} else {
				items.setLikeCount(items.getLikeCount() - 1);
			}
			itemsRepository.save(items);
		}
	}

	private void updatePlannerLikeCount(Long id, boolean isIncrement) {

		Planners planners = findPlannerById(id);

		if (planners != null) {
			if (isIncrement) {
				planners.setLikeCount(planners.getLikeCount() + 1);
			} else {
				planners.setLikeCount(planners.getLikeCount() - 1);
			}
			plannersRepository.save(planners);
		}
	}

	private void updateCompanyLikeCount(Long id, boolean isIncrement) {

		Companies companies = findCompanyById(id);

		if (companies != null) {
			if (isIncrement) {
				companies.setLikeCount(companies.getLikeCount() + 1);
			} else {
				companies.setLikeCount(companies.getLikeCount() - 1);
			}
			companiesRepository.save(companies);
		}
	}

	/*================== Repository 접근 ==================*/
	private List<UserLikes> getLikesByUsersAndTypeAndId(Users users, LikeEnum likeEnum, Long id) {
		return likeRepository.findByUsersAndLikeTypeAndLikedId(users, likeEnum, id);
	}

	public List<UserLikes> getLikesByUsersAndType(Users users, LikeEnum likeType) {
		return likeRepository.findByUsersAndLikeType(users, likeType);
	}

	private void saveLike(Users users, Long id, LikeEnum likeEnum) {
		UserLikes like = UserLikes.builder().users(users).likedId(id).likeEnum(likeEnum).build();
		likeRepository.save(like);
	}

	private void deleteLike(Users users, Long id, LikeEnum likeEnum) {
		likeRepository.deleteByUsersAndLikeTypeAndLikedId(users, likeEnum, id);
	}

	public Planners findPlannerById(Long id) {
		return plannersRepository.findById(id)
			.orElseThrow(PlannerNotFoundException::new);
	}

	public Portfolios findPortfolioById(Long id) {
		return portfoliosRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);
	}

	public Items findItemById(Long id) {
		return itemsRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);
	}

	public Companies findCompanyById(Long id) {
		return companiesRepository.findById(id)
			.orElseThrow(CompanyNotFoundException::new);
	}
}

