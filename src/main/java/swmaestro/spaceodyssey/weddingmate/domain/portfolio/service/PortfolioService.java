package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import java.util.Comparator;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemOrderDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.mapper.ItemsMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.service.ItemsRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper.PortfoliosMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional
public class PortfolioService {

	private final PortfoliosRepository portfoliosRepository;
	private final UsersRepositoryService usersRepositoryService;
	private final PortfolioRepositoryService portfolioRepositoryService;
	private final ItemsRepositoryService itemsRepositoryService;

	private final ItemsMapper itemMapper;
	private final PortfoliosMapper portfoliosMapper;

	private final PortfolioFileUploadService portfolioFileUploadService;

	public Long createPortfolio(Users users, MultipartFile multipartFile, PortfolioSaveReqDto portfolioSaveReqDto) {

		Portfolios portfolios = portfoliosMapper.dtoToEntity(users, portfolioSaveReqDto);

		portfoliosRepository.save(portfolios);

		updatePortfolioImage(multipartFile, portfolios);
		return portfolios.getPortfolioId();
	}

	public void updatePortfolioImage(MultipartFile multipartFile, Portfolios portfolios) {
		Files files = portfolioFileUploadService.uploadPortfolioFile(multipartFile, portfolios);
		portfolios.setFiles(files);
	}

	public Long updatePortfolio(Users users, Long portfolioId,
		PortfolioUpdateReqDto portfolioUpdateReqDto, MultipartFile multipartFile) {
		Portfolios portfolios = portfolioRepositoryService.findPortfolioById(portfolioId);

		//작성자가 아닌 user 예외처리
		verifyUserIsWriter(portfolios, users);

		portfolios.updatePortfolio(portfolioUpdateReqDto.getTitle(), portfolioUpdateReqDto.getRegion(),
			portfolioUpdateReqDto.getTags());

		if (multipartFile != null) {
			updatePortfolioImage(multipartFile, portfolios);
		}

		if (portfolioUpdateReqDto.getItemOrderList() != null) {
			updateItemOrder(portfolioUpdateReqDto.getItemOrderList(), portfolioId);
		}

		return portfolios.getPortfolioId();
	}

	public void updateItemOrder(List<ItemOrderDto> itemDtoList, Long portfolioId) {
		for (ItemOrderDto itemorderDto : itemDtoList) {
			Long itemId = itemorderDto.getItemId();
			int itemOrder = itemorderDto.getItemOrder();

			Items items = itemsRepositoryService.findItemById(itemId);

			checkItemBelongToPortfolio(items, portfolioId);
			items.updateOrder(itemOrder);
		}
	}

	public void deletePortfolio(Users users, Long portfolioId) {
		Portfolios portfolios = portfolioRepositoryService.findPortfolioById(portfolioId);

		//작성자가 아닌 user 예외처리
		verifyUserIsWriter(portfolios, users);

		portfolios.deletePortfolio();
		itemsRepositoryService.softDeleteItemByPortfolioId(portfolios.getPortfolioId());
	}

	public PortfolioDetailResDto getPortfolioDetail(Users users, Long id) {
		Portfolios portfolios = portfolioRepositoryService.findPortfolioById(id);

		Boolean isWriter = checkUserIsWriter(portfolios, users);

		List<ItemResDto> itemResDtoList = portfolios.getPortfolioItemsList().stream()
			.filter(item -> (Boolean.FALSE.equals(item.getIsDeleted())))
			.map(item -> itemMapper.entityToDto(users, item, isWriter))
			.sorted(Comparator.comparingInt(ItemResDto::getOrder))
			.toList();

		return portfoliosMapper.entityToDto(portfolios, itemResDtoList, isWriter, users);
	}

	public List<PortfolioListResDto> getPortfolioByUser(Long userId) {
		Users users = usersRepositoryService.findUserById(userId);

		return users.getPortfoliosList().stream()
			//삭제된 portfolio 제외
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(portfolio -> portfoliosMapper.entityToDto(users, portfolio))
			.toList();
	}

	/*================== 예외 처리 ==================*/
	public void verifyUserIsWriter(Portfolios portfolios, Users users) {
		if (!portfolios.getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}
	}

	public Boolean checkUserIsWriter(Portfolios portfolios, Users users) {
		return portfolios.getUsers().getUserId().equals(users.getUserId());
	}

	public void checkItemBelongToPortfolio(Items items, Long portfolioId) {
		if (!items.getPortfolios().getPortfolioId().equals(portfolioId)) {
			throw new ItemNotFoundException();
		}
	}
}