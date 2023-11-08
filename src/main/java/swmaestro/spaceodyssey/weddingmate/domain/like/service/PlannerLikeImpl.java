package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PlannerLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.PlannersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.global.config.aop.distributedLock.DistributedLock;

import java.util.List;

@Component
@RequiredArgsConstructor
public class PlannerLikeImpl implements LikesService {

	private final LikesRepositoryService likesRepositoryService;
	private final PlannersRepositoryService plannersRepositoryService;
	private final LikesMapper likesMapper;

	@Override
	@DistributedLock(key = "#lockName")
	public void increaseLikeCount(String lockName, Long id) {

		Planners planners = plannersRepositoryService.findPlannerById(id);
		planners.increaseLikeCount();
	}

	@Override
	@DistributedLock(key = "#lockName")
	public void decreaseLikeCount(String lockName, Long id) {

		Planners planners = plannersRepositoryService.findPlannerById(id);
		planners.decreaseLikeCount();
	}

	@Override
	@Transactional(readOnly = true)
	public List<PlannerLikeResDto> getUserLiked(Users users) {
		List<UserLikes> likeList = likesRepositoryService.getLikesByUsersAndType(users, LikeEnum.planner);

		return likeList.stream()
				.map(userLikes -> plannersRepositoryService.findPlannerById(userLikes.getLikedId()))
				.map(likesMapper::entityToDto)
				.toList();
	}
}