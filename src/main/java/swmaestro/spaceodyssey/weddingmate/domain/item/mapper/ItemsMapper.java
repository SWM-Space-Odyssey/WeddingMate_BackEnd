package swmaestro.spaceodyssey.weddingmate.domain.item.mapper;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSearchResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemsMapper {
	private final FilesRepository filesRepository;
	private final LikesRepository likesRepository;

	public ItemResDto entityToDto(Users users, Items item, Boolean isWriter) {
		List<String> imageList = filesRepository.findByItems(item).stream().map(Files::getUrl).toList();
		Boolean isItemLiked = isItemLikedByUser(users, item.getItemId());

		ItemResDto.ItemDetail itemDetail = ItemResDto.ItemDetail.builder()
			.itemTagList(item.getItemTagList())
			.itemRecord(item.getItemRecord())
			.date(item.getItemDate())
			.category(item.getCategory())
			.build();

		ItemResDto.ItemCompany itemCompany = createItemCompanyDto(item);

		return ItemResDto.builder()
			.portfolioId(item.getPortfolios().getPortfolioId())
			.itemDetail(itemDetail)
			.itemCompany(itemCompany)
			.order(item.getItemOrder())
			.itemId(item.getItemId())
			.imageList(imageList)
			.isWriter(isWriter)
			.isLiked(isItemLiked)
			.build();
	}

	private ItemResDto.ItemCompany createItemCompanyDto(Items item) {
		ItemResDto.ItemCompany.ItemCompanyBuilder builder = ItemResDto.ItemCompany.builder();

		if (item.getCompanies() != null) {
			builder.companyId(item.getCompanies().getCompanyId())
				.companyName(item.getCompanies().getName());
		} else {
			builder.companyId(null)
				.companyName(item.getCompanyName());
		}

		return builder.build();
	}


	public ItemSearchResDto entityToDto(Items item) {

		return ItemSearchResDto.builder()
			.url(item.getFilesList().get(0).getUrl())
			.itemId(item.getItemId())
			.build();
	}


	public Items dtoToEntity(Portfolios portfolio, ItemSaveReqDto itemSaveReqDto) {

		return Items.builder()
			.itemRecord(itemSaveReqDto.getItemRecord())
			.itemOrder(itemSaveReqDto.getOrder())
			.itemDate(itemSaveReqDto.getDate())
			.portfolios(portfolio)
			.category(itemSaveReqDto.getCategory())
			.itemTagList(itemSaveReqDto.getItemTagList())
			.build();
	}


	private boolean isItemLikedByUser(Users users, Long itemId) {
		return !likesRepository.findByUsersAndLikeTypeAndLikedId(users, LikeEnum.ITEM, itemId).isEmpty();
	}
}
