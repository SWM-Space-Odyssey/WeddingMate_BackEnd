package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;


import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface LikesService {
	void updateLikeCount(String lockName, Long id, boolean isIncrement);
	<T> List<T> getUserLiked(Users users);
}