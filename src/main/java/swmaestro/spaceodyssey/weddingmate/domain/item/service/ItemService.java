package swmaestro.spaceodyssey.weddingmate.domain.item.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FileRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.mapper.ItemMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotFoundException;
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
	private final FileRepository fileRepository;

	public void createItem(ItemSaveReqDto itemSaveReqDto) {
		Portfolio portfolio = findPortfolioById(itemSaveReqDto.getPortfolioId());

		Item item = itemMapper.dtoToEntity(portfolio, itemSaveReqDto);

		itemRepository.save(item);

		itemSaveReqDto.getImageList().stream().map(this::findFileByUrl).forEach(file -> file.setItem(item));

	}

	public ItemResDto findById(Users users, Long id) {
		Item item = findItemById(id);

		checkItemDeleted(item);

		Boolean isWriter = checkUserIsWriter(item, users);

		return itemMapper.entityToDto(item, isWriter);
	}

	public void updateItem(Users users, Long itemId, ItemUpdateReqDto itemUpdateReqDto) {
		Item item = findItemById(itemId);

		checkItemDeleted(item);

		verifyUserIsWriter(item, users);

		itemUpdateReqDto.getImageList().stream().map(this::findFileByUrl).forEach(file -> {
			file.setItem(item);
			fileRepository.saveAndFlush(file);
		});

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

		verifyUserIsWriter(item, users);

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

	public File findFileByUrl(String url) {
		return fileRepository.findByUrl(url)
			.orElseThrow(FileNotFoundException::new);
	}


	/*================== 예외 처리 ==================*/

	public void checkItemDeleted(Item item) {
		if (Boolean.TRUE.equals(item.getIsDeleted())) {
			throw new ItemNotFoundException();
		}
	}

	public void verifyUserIsWriter(Item item, Users users) {
		if (!item.getPortfolio().getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}
	}

	public Boolean checkUserIsWriter(Item item, Users users) {
		return item.getPortfolio().getUsers().getUserId().equals(users.getUserId());
	}
}