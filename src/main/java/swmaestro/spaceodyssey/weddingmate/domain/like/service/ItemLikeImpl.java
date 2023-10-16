package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.service.ItemsRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PortfolioLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor
public class ItemLikeImpl implements LikesService {
	private final LikesRepositoryService likesRepositoryService;
	private final ItemsRepositoryService itemsRepositoryService;
	private final LikesMapper likesMapper;
	@Override
	public void updateLikeCount(String lockName, Long id, boolean isIncrement) {

		Items items = itemsRepositoryService.findItemById(id);

		if (isIncrement) {
			items.setLikeCount(items.getLikeCount() + 1);
		} else {
			items.setLikeCount(items.getLikeCount() - 1);
		}
	}

	@Override
	public List<PortfolioLikeResDto> getUserLiked(Users users) {
		List<UserLikes> likeList = likesRepositoryService.getLikesByUsersAndType(users, LikeEnum.item);

		return likeList.stream()
			.map(userLikes -> itemsRepositoryService.findItemById(userLikes.getLikedId()))
			.filter(item -> (Boolean.FALSE.equals(item.getIsDeleted())))
			.map(likesMapper::entityToDto)
			.toList();
	}
}