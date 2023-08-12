package swmaestro.spaceodyssey.weddingmate.domain.item.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.mapper.ItemMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
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
	private final ItemMapper itemMapper;
	private final FileMapper fileMapper;

	public void createItem(ItemSaveReqDto itemSaveReqDto) {
		Portfolio portfolio = findPortfolioById(itemSaveReqDto.getPortfolioId());

		Item item = itemMapper.dtoToEntity(portfolio, itemSaveReqDto);

		itemSaveReqDto.getImageList().stream().map(fileMapper::urlToEntity).forEach(file -> file.setItem(item));

		itemRepository.save(item);
	}

	public ItemResDto findById(Long id) {
		Item item = findItemById(id);

		checkItemDeleted(item);

		return itemMapper.entityToDto(item);
	}

	public void updateItem(Users users, Long itemId, ItemUpdateReqDto itemUpdateReqDto) {
		Item item = findItemById(itemId);

		checkItemDeleted(item);

		checkUserIsWriter(item, users);

		itemUpdateReqDto.getImageList().stream().map(fileMapper::urlToEntity).forEach(file -> file.setItem(item));

		item.updateItem(
			itemUpdateReqDto.getItemRecord(),
			itemUpdateReqDto.getItemTagList(),
			itemUpdateReqDto.getCompany(),
			itemUpdateReqDto.getDate(),
			itemUpdateReqDto.getCategory()
		);
	}

	public void deleteItem(Users users, Long itemId) {
		Item item = findItemById(itemId);

		checkItemDeleted(item);

		checkUserIsWriter(item, users);

		item.deleteItem();
	}
	/*================== Repository 접근 ==================*/
	public Item findItemById(Long id) {
		return itemRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);
	}

	public Portfolio findPortfolioById(Long id) {
		return portfolioRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);
	}


	/*================== 예외 처리 ==================*/

	public Item checkItemDeleted(Item item) {
		if (Boolean.TRUE.equals(item.getIsDeleted())) {
			throw new ItemNotFoundException();
		}
		return item;
	}

	public void checkUserIsWriter(Item item, Users users) {
		if (!item.getPortfolio().getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}
	}
}