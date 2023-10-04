package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Service
@RequiredArgsConstructor
@Transactional
public class LikesRepositoryService {

	private final LikesRepository likeRepository;

	public List<UserLikes> getLikesByUsersAndTypeAndId(Users users, LikeEnum likeEnum, Long id) {
		return likeRepository.findByUsersAndLikeTypeAndLikedId(users, likeEnum, id);
	}

	public List<UserLikes> getLikesByUsersAndType(Users users, LikeEnum likeType) {
		return likeRepository.findByUsersAndLikeType(users, likeType);
	}

	public void saveLike(Users users, Long id, LikeEnum likeEnum) {
		UserLikes like = UserLikes.builder().users(users).likedId(id).likeEnum(likeEnum).build();
		likeRepository.save(like);
	}

	public void deleteLike(Users users, Long id, LikeEnum likeEnum) {
		likeRepository.deleteByUsersAndLikeTypeAndLikedId(users, likeEnum, id);
	}

	public void softDeleteLikesByUsers(Users users) {
		likeRepository.softDeleteByUser(users);
	}

	public Boolean isLiked(Users cUsers, LikeEnum likeType, Long likeId) {
		return !likeRepository.findByUsersAndLikeTypeAndLikedId(cUsers, likeType, likeId).isEmpty();
	}
}
