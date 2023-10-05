package swmaestro.spaceodyssey.weddingmate.domain.like.repository;

import java.util.Collection;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface LikesRepository extends JpaRepository<UserLikes, Long> {
	List<UserLikes> findByUsersAndLikeTypeAndLikedId(Users users, LikeEnum likeType, Long likeId);
	List<UserLikes> findByUsersAndLikeTypeIn(Users users, Collection<LikeEnum> likeTypes);
	void deleteByUsersAndLikeTypeAndLikedId(Users users, LikeEnum likeType, Long likeId);

	List<UserLikes> findByUsersAndLikeType(Users users, LikeEnum likeType);

	@Modifying
	@Query("UPDATE UserLikes userlike SET userlike.isDeleted = true WHERE userlike.users = :user")
	void softDeleteByUser(@Param("user") Users user);
}
