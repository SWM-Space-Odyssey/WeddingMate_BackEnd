package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.service.ItemsRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PortfolioLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.config.aop.distributedLock.DistributedLock;

import java.util.List;

@Component
@RequiredArgsConstructor
public class ItemLikeImpl implements LikesService {
	private final LikesRepositoryService likesRepositoryService;
	private final ItemsRepositoryService itemsRepositoryService;
	private final LikesMapper likesMapper;

	@Override
	@DistributedLock(key = "#lockName")
	public void increaseLikeCount(String lockName, Long id) {

		Items items = itemsRepositoryService.findItemById(id);
		items.increaseLikeCount();
	}

	@Override
	@DistributedLock(key = "#lockName")
	public void decreaseLikeCount(String lockName, Long id) {

		Items items = itemsRepositoryService.findItemById(id);
		items.decreaseLikeCount();
	}

	@Override
	@Transactional(readOnly = true)
	public List<PortfolioLikeResDto> getUserLiked(Users users) {
		List<UserLikes> likeList = likesRepositoryService.getLikesByUsersAndType(users, LikeEnum.item);

		return likeList.stream()
				.map(userLikes -> itemsRepositoryService.findItemById(userLikes.getLikedId()))
				.filter(item -> (Boolean.FALSE.equals(item.getIsDeleted())))
				.map(likesMapper::entityToDto)
				.toList();
	}
}