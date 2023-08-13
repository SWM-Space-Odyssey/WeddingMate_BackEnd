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

	public ItemResDto entityToDto(Item item) {

		List<String> imageList = fileRepository.findByItem(item).stream().map(File::getUrl).toList();

		return new ItemResDto(
			item.getItemRecord(),
			item.getCompany(),
			item.getItemDate(),
			item.getPortfolio().getPortfolioId(),
			item.getItemTagList(),
			item.getCategory(),
			item.getItemOrder(),
			item.getItemId(),
			imageList
		);
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
