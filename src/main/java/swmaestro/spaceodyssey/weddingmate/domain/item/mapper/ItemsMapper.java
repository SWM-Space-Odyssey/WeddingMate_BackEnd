package swmaestro.spaceodyssey.weddingmate.domain.item.mapper;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemsMapper {
	private final FilesRepository fileRepository;
	private final LikesRepository likeRepository;

	public ItemResDto entityToDto(Users users, Items items, Boolean isWriter) {

		List<String> imageList = fileRepository.findByItems(items).stream().map(Files::getUrl).toList();
		Boolean isItemLiked = isItemLikedByUser(users, items.getItemId());

		ItemResDto.ItemDetail itemDetail = ItemResDto.ItemDetail.builder()
			.itemTagList(items.getItemTagList())
			.itemRecord(items.getItemRecord())
			.date(items.getItemDate())
			.category(items.getCategory())
			.company(items.getCompany())
			.build();

		return ItemResDto.builder()
			.portfolioId(items.getPortfolios().getPortfolioId())
			.itemDetail(itemDetail)
			.order(items.getItemOrder())
			.itemId(items.getItemId())
			.imageList(imageList)
			.isWriter(isWriter)
			.isLiked(isItemLiked)
			.build();
	}

	public Items dtoToEntity(Portfolios portfolios, ItemSaveReqDto itemSaveReqDto) {

		return Items.builder()
			.company(itemSaveReqDto.getCompany())
			.itemRecord(itemSaveReqDto.getItemRecord())
			.itemOrder(itemSaveReqDto.getOrder())
			.itemDate(itemSaveReqDto.getDate())
			.portfolios(portfolios)
			.category(itemSaveReqDto.getCategory())
			.itemTagList(itemSaveReqDto.getItemTagList())
			.build();
	}

	private boolean isItemLikedByUser(Users users, Long itemId) {
		return !likeRepository.findByUsersAndLikeTypeAndLikedId(users, LikeEnum.ITEM, itemId).isEmpty();
	}
}
