package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import java.util.Comparator;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.item.mapper.ItemMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemOrderDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper.PortfolioMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
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

	private final PortfolioRepository portfolioRepository;
	private final UsersRepository usersRepository;
	private final ItemRepository itemRepository;

	private final ItemMapper itemMapper;
	private final PortfolioMapper portfolioMapper;

	private final PortfolioFileUploadService portfolioFileUploadService;

	public Long createPortfolio(Users users, MultipartFile multipartFile, PortfolioSaveReqDto portfolioSaveReqDto) {

		Portfolio portfolio = portfolioMapper.dtoToEntity(users, portfolioSaveReqDto);

		portfolioRepository.save(portfolio);

		updatePortfolioImage(multipartFile, portfolio);
		return portfolio.getPortfolioId();
	}

	public void updatePortfolioImage(MultipartFile multipartFile, Portfolio portfolio) {
		File file = portfolioFileUploadService.uploadPortfolioFile(multipartFile, portfolio);
		portfolio.setFile(file);
	}

	public Long updatePortfolio(Users users, Long portfolioId,
		PortfolioUpdateReqDto portfolioUpdateReqDto, MultipartFile multipartFile) {
		Portfolio portfolio = findPortfolioById(portfolioId);

		checkPortfolioDeleted(portfolio);

		//작성자가 아닌 user 예외처리
		verifyUserIsWriter(portfolio, users);

		portfolio.updatePortfolio(portfolioUpdateReqDto.getTitle(), portfolioUpdateReqDto.getRegion(), portfolioUpdateReqDto.getTags());

		if (multipartFile != null) {
			updatePortfolioImage(multipartFile, portfolio);
		}

		if (portfolioUpdateReqDto.getItemOrderList() != null) {
			updateItemOrder(portfolioUpdateReqDto.getItemOrderList(), portfolioId);
		}

		return portfolio.getPortfolioId();
	}

	public void updateItemOrder(List<ItemOrderDto> itemDtoList, Long portfolioId) {
		for (ItemOrderDto itemorderDto : itemDtoList) {
			Long itemId = itemorderDto.getItemId();
			int itemOrder = itemorderDto.getItemOrder();

			Item item = findItemById(itemId);

			checkItemBelongToPortfolio(item, portfolioId);
			item.updateOrder(itemOrder);
		}
	}

	public void deletePortfolio(Users users, Long portfolioId) {
		Portfolio portfolio = this.findPortfolioById(portfolioId);

		checkPortfolioDeleted(portfolio);

		//작성자가 아닌 user 예외처리
		verifyUserIsWriter(portfolio, users);

		portfolio.deletePortfolio();

		portfolio.getPortfolioItemList().forEach(Item::deleteItem);
	}

	public PortfolioDetailResDto getPortfolioDetail(Users users, Long id) {
		Portfolio portfolio = findPortfolioById(id);

		checkPortfolioDeleted(portfolio);

		Boolean isWriter = checkUserIsWriter(portfolio, users);

		List<ItemResDto> itemResDtoList = portfolio.getPortfolioItemList().stream()
			.filter(item -> (Boolean.FALSE.equals(item.getIsDeleted())))
			.map(item -> itemMapper.entityToDto(item, isWriter))
			.sorted(Comparator.comparingInt(ItemResDto::getOrder))
			.toList();

		return portfolioMapper.entityToDto(portfolio, itemResDtoList, isWriter);
	}

	public List<PortfolioListResDto> getPortfolioByUser(Long userId) {
		Users users = findUserById(userId);

		return users.getPortfolioList().stream()
			//삭제된 portfolio 제외
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(portfolioMapper::entityToDto)
			.toList();
	}

	/*================== Repository 접근 ==================*/
	public Users findUserById(Long id) {
		return usersRepository.findById(id)
			.orElseThrow(UserNotFoundException::new);
	}
	public Portfolio findPortfolioById(Long id) {
		return portfolioRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);
	}

	public Item findItemById(Long id) {
		return itemRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);
	}
	/*================== 예외 처리 ==================*/
	public Portfolio checkPortfolioDeleted(Portfolio portfolio) {
		if (Boolean.TRUE.equals(portfolio.getIsDeleted())) {
			throw new PortfolioNotFoundException();
		}
		return portfolio;
	}

	public void verifyUserIsWriter(Portfolio portfolio, Users users) {
		if (!portfolio.getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}
	}

	public Boolean checkUserIsWriter(Portfolio portfolio, Users users) {
		return portfolio.getUsers().getUserId().equals(users.getUserId());
	}

	public void checkItemBelongToPortfolio(Item item, Long portfolioId) {
		if (!item.getPortfolio().getPortfolioId().equals(portfolioId)) {
			throw new ItemNotFoundException();
		}
	}
}