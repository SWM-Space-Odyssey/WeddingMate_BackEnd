package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.ItemTag;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemMapper {
	private final ItemRepository itemRepository;

	public ItemResDto entityToDto(Item item) {

		return new ItemResDto(
			item.getItemRecord(),
			item.getCompany(),
			item.getItemDate(),
			item.getPortfolio().getPortfolioId(),
			item.getItemTagList().stream().map(ItemTag::getTag).map(Tag::getContent).toList(),
			item.getCategory().getCategoryId(),
			item.getItemOrder(),
			item.getItemId()
		);
	}
}
