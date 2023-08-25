package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PortfolioLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLike;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikeMapper;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikeRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Transactional
@Service
@RequiredArgsConstructor
public class LikeService {

	private final LikeMapper likeMapper;
	private final LikeRepository likeRepository;


	public List<PortfolioLikeResDto> getUserLikedPortfolio(Users users) {
		Collection<LikeEnum> likeTypes = Arrays.asList(LikeEnum.PORTFOLIO, LikeEnum.ITEM);

		return likeRepository.findByUsersAndLikeTypeIn(users, likeTypes).stream()
			.map(likeMapper::entityToDto)
			.toList();
	}

	public boolean like(Long id, Users users, LikeEnum likeEnum) {
		List<UserLike> likeList = likeRepository.findByUsersAndLikeTypeAndLikedId(users, likeEnum, id);

		if (likeList.isEmpty()) {
			UserLike like = UserLike.builder().users(users).likedId(id).likeEnum(likeEnum).build();
			likeRepository.save(like);
			return true;
		}

		likeRepository.deleteByUsersAndLikeTypeAndLikedId(users, likeEnum, id);
		return false;
	}
}

