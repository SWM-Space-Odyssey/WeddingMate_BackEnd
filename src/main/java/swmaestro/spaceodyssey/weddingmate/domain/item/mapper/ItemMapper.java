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
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemMapper {
	private final FileRepository fileRepository;

	public ItemResDto entityToDto(Item item, Boolean isWriter) {

		List<String> imageList = fileRepository.findByItem(item).stream().map(File::getUrl).toList();

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
}
