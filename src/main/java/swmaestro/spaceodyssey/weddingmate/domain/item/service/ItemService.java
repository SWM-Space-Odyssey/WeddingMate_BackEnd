package swmaestro.spaceodyssey.weddingmate.domain.item.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.dto.CategoryMapper;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.ItemTag;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagMapper;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

@Service
@RequiredArgsConstructor
@Transactional
public class ItemService {
	private final ItemRepository itemRepository;
	private final PortfolioRepository portfolioRepository;
	private final TagMapper tagMapper;
	private final ItemMapper itemMapper;
	private final CategoryMapper categoryMapper;
	private final FileMapper fileMapper;

	public void createItem(ItemSaveReqDto itemSaveReqDto) {
		List<ItemTag> tagList = new ArrayList<>();

		Portfolio portfolio = portfolioRepository.findById(itemSaveReqDto.getPortfolioId())
			.orElseThrow(PortfolioNotFoundException::new);

		Category category = categoryMapper.findCategoryByContentOrElseCreate(itemSaveReqDto.getCategoryContent());

		Item item = Item.builder()
			.company(itemSaveReqDto.getCompany())
			.itemRecord(itemSaveReqDto.getItemRecord())
			.itemOrder(itemSaveReqDto.getOrder())
			.itemDate(itemSaveReqDto.getDate())
			.portfolio(portfolio)
			.category(category)
			.build();

		itemSaveReqDto.getItemTagList().forEach(itemTag -> {
			Tag tag = tagMapper.contentToEntity(itemTag, category);
			tagList.add(new ItemTag(item, tag));
		});
		itemSaveReqDto.getImageList().stream().map(fileMapper::urlToEntity).forEach(file -> file.setItem(item));

		item.setItemTag(tagList);

		itemRepository.save(item);
	}

	public ItemResDto findById(Long id) {
		Item item = this.findItemById(id);

		checkItemDeleted(item);

		return itemMapper.entityToDto(item);
	}

	public void update(Users users, Long itemId, ItemUpdateReqDto itemUpdateReqDto) {
		Item item = this.findItemById(itemId);

		checkItemDeleted(item);

		if (!item.getPortfolio().getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}

		List<ItemTag> tagList = new ArrayList<>();

		Category category = categoryMapper.findCategoryByContentOrElseCreate(itemUpdateReqDto.getCategoryContent());

		itemUpdateReqDto.getItemTagList().forEach(itemTag -> {
			Tag tag = this.tagMapper.contentToEntity(itemTag, category);
			tagList.add(new ItemTag(item, tag));
		});

		itemUpdateReqDto.getImageList().stream().map(fileMapper::urlToEntity).forEach(file -> file.setItem(item));

		item.updateItem(
			itemUpdateReqDto.getItemRecord(),
			tagList,
			itemUpdateReqDto.getCompany(),
			itemUpdateReqDto.getDate(),
			category
		);
	}

	public void delete(Users users, Long itemId) {
		Item item = this.findItemById(itemId);

		checkItemDeleted(item);

		if (!item.getPortfolio().getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}

		item.deleteItem();
	}

	public Item findItemById(Long id) {
		return itemRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);
	}

	public Item checkItemDeleted(Item item) {
		if (Boolean.TRUE.equals(item.getIsDeleted())) {
			throw new ItemNotFoundException();
		}
		return item;
	}
}