package swmaestro.spaceodyssey.weddingmate.domain.item.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.mapper.ItemsMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

@Service
@RequiredArgsConstructor
@Transactional
public class ItemsService {

	private final ItemsRepository itemsRepository;
	private final PortfoliosRepository portfoliosRepository;
	private final FilesRepository fileRepository;
	private final PlannersRepository plannersRepository;

	private final ItemsMapper itemMapper;

	public void createItem(ItemSaveReqDto itemSaveReqDto) {
		Portfolios portfolios = findPortfolioById(itemSaveReqDto.getPortfolioId());

		Items items = itemMapper.dtoToEntity(portfolios, itemSaveReqDto);

		itemsRepository.save(items);

		itemSaveReqDto.getImageList().stream().map(this::findFileByUrl).forEach(file -> file.setItems(items));
	}

	public ItemResDto findById(Users users, Long id) {
		Items items = findItemById(id);

		checkItemDeleted(items);

		Planners planners = findPlannerByUsers(users);

		Boolean isWriter = checkUserIsWriter(items, planners);

		return itemMapper.entityToDto(users, items, isWriter);
	}

	public void updateItem(Users users, Long itemId, ItemUpdateReqDto itemUpdateReqDto) {
		Items items = findItemById(itemId);

		checkItemDeleted(items);

		Planners planners = findPlannerByUsers(users);

		verifyUserIsWriter(items, planners);

		items.getFilesList().forEach(file -> file.setItems(null));

		itemUpdateReqDto.getImageList().stream().map(this::findFileByUrl).forEach(file -> file.setItems(items));

		items.updateItem(
			itemUpdateReqDto.getItemRecord(),
			itemUpdateReqDto.getItemTagList(),
			itemUpdateReqDto.getCompany(),
			itemUpdateReqDto.getDate(),
			itemUpdateReqDto.getCategory()
		);
	}

	public void deleteItem(Users users, Long itemId) {
		Items items = findItemById(itemId);

		checkItemDeleted(items);

		Planners planners = findPlannerByUsers(users);

		verifyUserIsWriter(items, planners);

		items.deleteItem();
	}

	/*================== Repository 접근 ==================*/
	public Items findItemById(Long id) {
		return itemsRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);
	}

	public Portfolios findPortfolioById(Long id) {
		return portfoliosRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);
	}

	public Files findFileByUrl(String url) {
		return fileRepository.findByUrl(url)
			.orElseThrow(FileNotFoundException::new);
	}

	public Planners findPlannerByUsers(Users users) {
		return plannersRepository.findByUsers(users)
			.orElseThrow(PlannerNotFoundException::new);
	}

	/*================== 예외 처리 ==================*/

	public void checkItemDeleted(Items items) {
		if (Boolean.TRUE.equals(items.getIsDeleted())) {
			throw new ItemNotFoundException();
		}
	}

	public void verifyUserIsWriter(Items items, Planners planners) {
		if (!items.getPortfolios().getPlanners().getPlannerId().equals(planners.getPlannerId())) {
			throw new UserUnAuthorizedException();
		}
	}

	public Boolean checkUserIsWriter(Items items, Planners planners) {
		return items.getPortfolios().getPlanners().getPlannerId().equals(planners.getPlannerId());
	}
}