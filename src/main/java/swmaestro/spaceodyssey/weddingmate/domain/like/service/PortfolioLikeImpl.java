package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PortfolioLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.service.PortfolioRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.config.aop.DistributedLock.DistributedLock;

@Component
@RequiredArgsConstructor
public class PortfolioLikeImpl implements LikeService {

	private final LikesRepositoryService likesRepositoryService;
	private final PortfolioRepositoryService portfolioRepositoryService;
	private final LikesMapper likesMapper;

	@Override
	@DistributedLock(key="#lockName")
	public void updateLikeCount(String lockName, Long id, boolean isIncrement) {

		Portfolios portfolios = portfolioRepositoryService.findPortfolioById(id);
		if (isIncrement) {
			portfolios.setLikeCount(portfolios.getLikeCount() + 1);
		} else {
			portfolios.setLikeCount(portfolios.getLikeCount() - 1);
		}
	}

	@Override
	public List<PortfolioLikeResDto> getUserLiked(Users users) {
		List<UserLikes> likeList = likesRepositoryService.getLikesByUsersAndType(users, LikeEnum.portfolio);

		return likeList.stream()
			.map(userLikes -> portfolioRepositoryService.findPortfolioById(userLikes.getLikedId()))
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(likesMapper::entityToDto)
			.toList();
	}
}
