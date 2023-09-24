package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import java.util.Comparator;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.mapper.ItemsMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemOrderDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper.PortfoliosMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.CustomersRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional
public class PortfolioService {

	private final PortfoliosRepository portfoliosRepository;
	private final ItemsRepository itemsRepository;
	private final CustomersRepository customersRepository;
	private final UsersRepository usersRepository;

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
		Portfolios portfolios = findPortfolioById(portfolioId);

		checkPortfolioDeleted(portfolios);

		//작성자가 아닌 user 예외처리
		verifyUserIsWriter(portfolios, users);

		portfolios.updatePortfolio(portfolioUpdateReqDto.getTitle(), portfolioUpdateReqDto.getRegion(), portfolioUpdateReqDto.getTags());

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

			Items items = findItemById(itemId);

			checkItemBelongToPortfolio(items, portfolioId);
			items.updateOrder(itemOrder);
		}
	}

	public void deletePortfolio(Users users, Long portfolioId) {
		Portfolios portfolios = findPortfolioById(portfolioId);

		checkPortfolioDeleted(portfolios);

		//작성자가 아닌 user 예외처리
		verifyUserIsWriter(portfolios, users);

		portfolios.deletePortfolio();

		portfolios.getPortfolioItemsList().forEach(Items::deleteItem);
	}

	public PortfolioDetailResDto getPortfolioDetail(Users users, Long id) {
		Portfolios portfolios = findPortfolioById(id);

		checkPortfolioDeleted(portfolios);

		Boolean isWriter = checkUserIsWriter(portfolios, users);

		List<ItemResDto> itemResDtoList = portfolios.getPortfolioItemsList().stream()
			.filter(item -> (Boolean.FALSE.equals(item.getIsDeleted())))
			.map(item -> itemMapper.entityToDto(users, item, isWriter))
			.sorted(Comparator.comparingInt(ItemResDto::getOrder))
			.toList();

		return portfoliosMapper.entityToDto(portfolios, itemResDtoList, isWriter, users);
	}

	public List<PortfolioListResDto> getPortfolioByUser(Long userId) {
		Users users = findUserById(userId);

		return users.getPortfoliosList().stream()
			//삭제된 portfolio 제외
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(portfolio -> portfoliosMapper.entityToDto(users, portfolio))
			.toList();
	}

	/*================== Repository 접근 ==================*/
	public Users findUserById(Long id) {
		return usersRepository.findById(id)
			.orElseThrow(UserNotFoundException::new);
	}
	public Portfolios findPortfolioById(Long id) {
		return portfoliosRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);
	}

	public Items findItemById(Long id) {
		return itemsRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);
	}

	/*================== 예외 처리 ==================*/
	public Portfolios checkPortfolioDeleted(Portfolios portfolios) {
		if (Boolean.TRUE.equals(portfolios.getIsDeleted())) {
			throw new PortfolioNotFoundException();
		}
		return portfolios;
	}

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