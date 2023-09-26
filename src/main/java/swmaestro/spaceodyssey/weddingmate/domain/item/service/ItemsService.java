package swmaestro.spaceodyssey.weddingmate.domain.item.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.company.service.CompaniesService;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSearchResDto;
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
	private final CompaniesService companiesService;

	private final ItemsRepository itemsRepository;
	private final PortfoliosRepository portfoliosRepository;
	private final FilesRepository fileRepository;
	private final PlannersRepository plannersRepository;

	private final ItemsMapper itemsMapper;

	/*================== 아이템 생성 ==================*/
	public void createItem(ItemSaveReqDto itemSaveReqDto) {
		Portfolios portfolios = findPortfolioById(itemSaveReqDto.getPortfolioId());

		Items items = itemsMapper.dtoToEntity(portfolios, itemSaveReqDto);

		itemsRepository.save(items);

		setItemCompanies(items, itemSaveReqDto);

		setItemImages(items, itemSaveReqDto);
	}

	private void setItemCompanies(Items items, ItemSaveReqDto itemSaveReqDto) {
		if (itemSaveReqDto.getCompanyId() != null) {
			Companies companies = companiesService.findCompanyById(itemSaveReqDto.getCompanyId());
			items.setCompanies(companies);
			items.setCompanyName(companies.getName());
		} else if (itemSaveReqDto.getCompanyName() != null) {
			items.setCompanies(null);
			items.setCompanyName(itemSaveReqDto.getCompanyName());
		} else {
			items.setCompanyName(null);
			items.setCompanies(null);
		}
	}

	private void setItemImages(Items items, ItemSaveReqDto itemSaveReqDto) {
		// 이미지 정보 업데이트
		itemSaveReqDto.getImageList()
			.stream()
			.map(this::findFileByUrl)
			.forEach(file -> file.setItems(items));
	}

	/*================== 아이템 업데이트 ==================*/
	public void updateItem(Users users, Long itemId, ItemUpdateReqDto itemUpdateReqDto) {
		Items items = findItemById(itemId);

		updateItemFields(items, itemUpdateReqDto);

		checkItemDeleted(items);
		verifyUserIsWriter(items, users);

		updateItemImages(items, itemUpdateReqDto);
	}

	private void updateItemFields(Items items, ItemUpdateReqDto itemUpdateReqDto) {
		// Items 엔티티의 필드 업데이트 로직
		items.updateItem(itemUpdateReqDto);

		// companyId 또는 company 정보 업데이트 로직
		if (itemUpdateReqDto.getCompanyId() != null) {
			Companies companies = companiesService.findCompanyById(itemUpdateReqDto.getCompanyId());
			items.setCompanies(companies);
			items.setCompanyName(companies.getName());
		} else if (itemUpdateReqDto.getCompanyName() != null) {
			items.setCompanies(null);
			items.setCompanyName(itemUpdateReqDto.getCompanyName());
		} else {
			items.setCompanyName(null);
			items.setCompanies(null);
		}
	}

	private void updateItemImages(Items items, ItemUpdateReqDto itemUpdateReqDto) {
		// 이미지 정보 업데이트 로직
		items.getFilesList().forEach(file -> file.setItems(null));
		itemUpdateReqDto.getImageList().stream()
			.map(this::findFileByUrl)
			.forEach(file -> file.setItems(items));
	}

	/*================== 그 외 함수들 ==================*/
	public ItemResDto findById(Users users, Long id) {
		Items items = findItemById(id);

		checkItemDeleted(items);

		Boolean isWriter = checkUserIsWriter(items, users);

		return itemsMapper.entityToDto(users, items, isWriter);
	}

	public void deleteItem(Users users, Long itemId) {
		Items items = findItemById(itemId);

		checkItemDeleted(items);

		verifyUserIsWriter(items, users);

		items.deleteItem();
	}

	public Page<ItemSearchResDto> searchItems(Pageable pageable, String keyword) {
		List<Items> itemsList = new ArrayList<>();

		// 키워드 필터링
		if (keyword != null) {
			itemsList = itemsRepository.searchItemsByKeyword(keyword);
		}

		List<ItemSearchResDto> itemSearchResDtoList = itemsList.stream().map(itemsMapper::entityToDto).toList();
		int start = (int) pageable.getOffset();
		int end = Math.min((start + pageable.getPageSize()), itemSearchResDtoList.size());

		return new PageImpl<>(itemSearchResDtoList.subList(start, end), pageable, itemSearchResDtoList.size());
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
	public void verifyUserIsWriter(Items items, Users users) {
		if (!items.getPortfolios().getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}
	}

	public Boolean checkUserIsWriter(Items items, Users users) {
		return items.getPortfolios().getUsers().getUserId().equals(users.getUserId());
	}
}