package swmaestro.spaceodyssey.weddingmate.domain.like.repository;

import java.util.Collection;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLike;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface LikeRepository extends JpaRepository<UserLike, Long> {
	List<UserLike> findByUsersAndLikeTypeAndLikedId(Users users, LikeEnum likeType, Long likeId);
	List<UserLike> findByUsersAndLikeTypeIn(Users users, Collection<LikeEnum> likeTypes);
	void deleteByUsersAndLikeTypeAndLikedId(Users users, LikeEnum likeType, Long likeId);

	List<UserLike> findByUsersAndLikeType(Users users, LikeEnum likeEnum);
}
