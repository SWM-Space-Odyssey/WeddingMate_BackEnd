package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.config.aop.DistributedLock.DistributedLock;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class LikesService {

	private final LikesRepositoryService likesRepositoryService;
	private final Map<String, LikeService> likeServiceMap;

	private static final String REDISSON_LOCK_PREFIX = "Id:";

	@DistributedLock(key = "#lockName")
	public boolean like(Long id, Users users, LikeEnum likeEnum) {
		List<UserLikes> likeList = likesRepositoryService.getLikesByUsersAndTypeAndId(users, likeEnum, id);

		LikeService likeService = likeServiceMap.get(likeEnum.getServiceName());

		String key = REDISSON_LOCK_PREFIX + String.valueOf(id);

		if (likeList.isEmpty()) {
			likesRepositoryService.saveLike(users, id, likeEnum);
			likeService.updateLikeCount(key, id, true);
			return true;
		}

		likesRepositoryService.deleteLike(users, id, likeEnum);
		likeService.updateLikeCount(key, id, false);
		return false;
	}
}
