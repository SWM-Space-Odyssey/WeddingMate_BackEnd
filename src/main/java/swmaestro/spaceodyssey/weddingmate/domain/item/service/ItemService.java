package swmaestro.spaceodyssey.weddingmate.domain.item.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.category.repository.CategoryRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.ItemTag;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagMapper;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.global.exception.category.CategoryNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class ItemService {
	private final ItemRepository itemRepository;
	private final CategoryRepository categoryRepository;
	private final PortfolioRepository portfolioRepository;
	private final TagMapper tagMapper;
	private final ItemMapper itemMapper;

	public void createItem(ItemSaveReqDto itemSaveReqDto) {
		List<ItemTag> tagList = new ArrayList<>();

		Portfolio portfolio = portfolioRepository.findById(itemSaveReqDto.getPortfolioId())
			.orElseThrow(PortfolioNotFoundException::new);

		Category category = categoryRepository.findById(itemSaveReqDto.getCategoryId())
			.orElseThrow(CategoryNotFoundException::new);

		Item item = Item.builder()
			.company(itemSaveReqDto.getCompany())
			.itemRecord(itemSaveReqDto.getItemRecord())
			.itemOrder(itemSaveReqDto.getOrder())
			.itemDate(itemSaveReqDto.getDate())
			.portfolio(portfolio)
			.category(category)
			.build();

		itemSaveReqDto.getItemTagList().forEach(itemTag -> {
			Tag tag = this.tagMapper.contentToEntity(itemTag);
			tagList.add(new ItemTag(item, tag));
		});

		item.setItemTag(tagList);
		itemRepository.save(item);
	}

	public ItemResDto findById(Long id) {
		Item item = itemRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);

		return itemMapper.entityToDto(item);
	}
}
