package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.ItemTag;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemMapper {
	private final ItemRepository itemRepository;

	public ItemResDto entityToDto(Item item) {

		return ItemResDto.builder()
			.itemRecord(item.getItemRecord())
			.date(item.getItemDate())
			.company(item.getCompany())
			.order(item.getItemOrder())
			.portfolioId(item.getPortfolio().getPortfolioId())
			.categoryId(item.getCategory().getCategoryId())
			.itemTagList(item.getItemTagList().stream().map(ItemTag::getTag).map(Tag::getContent).toList())
			.build();
	}
}
