package swmaestro.spaceodyssey.weddingmate.domain.item.mapper;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FileRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikeRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemMapper {
	private final FileRepository fileRepository;
	private final LikeRepository likeRepository;

	public ItemResDto entityToDto(Users users, Item item, Boolean isWriter) {

		List<String> imageList = fileRepository.findByItem(item).stream().map(File::getUrl).toList();
		Boolean isItemLiked = isItemLikedByUser(users, item.getItemId());

		ItemResDto.ItemDetail itemDetail = ItemResDto.ItemDetail.builder()
			.itemTagList(item.getItemTagList())
			.itemRecord(item.getItemRecord())
			.date(item.getItemDate())
			.category(item.getCategory())
			.company(item.getCompany())
			.build();

		return ItemResDto.builder()
			.portfolioId(item.getPortfolio().getPortfolioId())
			.itemDetail(itemDetail)
			.order(item.getItemOrder())
			.itemId(item.getItemId())
			.imageList(imageList)
			.isWriter(isWriter)
			.isLiked(isItemLiked)
			.build();
	}

	public Item dtoToEntity(Portfolio portfolio, ItemSaveReqDto itemSaveReqDto) {

		return Item.builder()
			.company(itemSaveReqDto.getCompany())
			.itemRecord(itemSaveReqDto.getItemRecord())
			.itemOrder(itemSaveReqDto.getOrder())
			.itemDate(itemSaveReqDto.getDate())
			.portfolio(portfolio)
			.category(itemSaveReqDto.getCategory())
			.itemTagList(itemSaveReqDto.getItemTagList())
			.build();
	}

	private boolean isItemLikedByUser(Users users, Long itemId) {
		return !likeRepository.findByUsersAndLikeTypeAndLikedId(users, LikeEnum.ITEM, itemId).isEmpty();
	}
}
