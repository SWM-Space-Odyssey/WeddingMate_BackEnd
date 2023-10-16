package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PlannerLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.PlannersRepositoryService;

@Component
@RequiredArgsConstructor
public class PlannerLikeImpl implements LikesService {

	private final LikesRepositoryService likesRepositoryService;
	private final PlannersRepositoryService plannersRepositoryService;
	private final LikesMapper likesMapper;

	@Override
	public void updateLikeCount(String lockName, Long id, boolean isIncrement) {
		Planners planners = plannersRepositoryService.findPlannerById(id);

		if (isIncrement) {
			planners.setLikeCount(planners.getLikeCount() + 1);
		} else {
			planners.setLikeCount(planners.getLikeCount() - 1);
		}
	}

	@Override
	public List<PlannerLikeResDto> getUserLiked(Users users) {
		List<UserLikes> likeList = likesRepositoryService.getLikesByUsersAndType(users, LikeEnum.planner);

		return likeList.stream()
			.map(userLikes -> plannersRepositoryService.findPlannerById(userLikes.getLikedId()))
			.map(likesMapper::entityToDto)
			.toList();
	}
}