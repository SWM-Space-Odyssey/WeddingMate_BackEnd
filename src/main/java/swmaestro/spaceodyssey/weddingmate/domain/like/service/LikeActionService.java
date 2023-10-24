package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Slf4j
@Service
@RequiredArgsConstructor
public class LikeActionService {

	private final LikesRepositoryService likesRepositoryService;
	private final Map<String, LikesService> likeServiceMap;

	private static final String REDISSON_LOCK_PREFIX = "Id:";

	@Transactional(readOnly = true)
	public boolean checkIsLiked(Long id, Users users, LikeEnum likeEnum) {
		List<UserLikes> likeList = likesRepositoryService.getLikesByUsersAndTypeAndId(users, likeEnum, id);
		return !likeList.isEmpty();
	}

	@Transactional
	public void like(Long id, Users users, LikeEnum likeEnum) {
		likesRepositoryService.saveLike(users, id, likeEnum);
		String key = REDISSON_LOCK_PREFIX + id;
		likeServiceMap.get(likeEnum.getServiceName()).increaseLikeCount(key, id);
	}

	@Transactional
	public void unlike(Long id, Users users, LikeEnum likeEnum) {
		likesRepositoryService.deleteLike(users, id, likeEnum);
		String key = REDISSON_LOCK_PREFIX + id;
		likeServiceMap.get(likeEnum.getServiceName()).decreaseLikeCount(key, id);
	}
}