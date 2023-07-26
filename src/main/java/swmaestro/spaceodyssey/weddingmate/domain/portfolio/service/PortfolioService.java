package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.dto.CategoryMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.PortfolioTag;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagMapper;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagResDto;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

@Service
@RequiredArgsConstructor
@Transactional
public class PortfolioService {
	private final PortfolioRepository portfolioRepository;
	private final UsersRepository usersRepository;
	private final ItemRepository itemRepository;
	private final CategoryMapper categoryMapper;
	private final TagMapper tagMapper;
	private final ItemMapper itemMapper;
	private final PortfolioMapper portfolioMapper;

	public void createPortfolio(Users users, PortfolioSaveReqDto portfolioSaveReqDto) {
		List<PortfolioTag> tagList = new ArrayList<>();

		Portfolio portfolio = Portfolio.builder()
			.title(portfolioSaveReqDto.getTitle())
			.users(users)
			.build();

		portfolioSaveReqDto.getTags().forEach(portfolioTag -> {
			Tag tag = this.tagMapper.contentToEntity(portfolioTag, categoryMapper.findCategoryByContentOrElseCreate("분위기"));
			tagList.add(new PortfolioTag(portfolio, tag));
		});

		portfolio.setPortfolioTag(tagList);
		portfolioRepository.save(portfolio);
	}

	public PortfolioDetailResDto findById(Long id) {
		Portfolio portfolio = findPortfolioById(id);

		checkPortfolioDeleted(portfolio);

		List<TagResDto> tagList = portfolio.getPortfolioTagList().stream()
			.map(PortfolioTag::getTag)
			.map(tagMapper::entityToDto)
			.toList();

		List<ItemResDto> itemResDtoList = portfolio.getPortfolioItemList().stream()
			.filter(item -> (Boolean.FALSE.equals(item.getIsDeleted())))
			.map(itemMapper::entityToDto)
			.toList();

		return PortfolioDetailResDto.builder()
			.title(portfolio.getTitle())
			.id(portfolio.getPortfolioId())
			.tagResDtoList(tagList)
			.itemResDtoList(itemResDtoList)
			.build();
	}

	public List<PortfolioListResDto> findByUser(Long userId) {
		Users users = usersRepository.findById(userId)
			.orElseThrow(UserNotFoundException::new);

		return users.getPortfolioList().stream()
			//삭제된 portfolio 제외
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(portfolioMapper::entityToDto)

			.toList();
	}

	public void updatePortfolio(Users users, Long portfolioId, PortfolioUpdateReqDto portfolioUpdateReqDto) {
		Portfolio portfolio = this.findPortfolioById(portfolioId);

		checkPortfolioDeleted(portfolio);

		//작성자가 아닌 user 예외처리
		if (!portfolio.getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}

		List<PortfolioTag> tagList = new ArrayList<>();

		portfolioUpdateReqDto.getTags().forEach(portfolioTag -> {
			Tag tag = tagMapper.contentToEntity(portfolioTag, categoryMapper.findCategoryByContentOrElseCreate("분위기"));
			tagList.add(new PortfolioTag(portfolio, tag));
		});

		portfolio.updatePortfolio(portfolioUpdateReqDto.getTitle(), tagList);
		portfolioRepository.save(portfolio);

		List<Integer> orderList = portfolioUpdateReqDto.getOrderList();
		List <Long> itemList = portfolioUpdateReqDto.getItemList();

		for (int i = 0; i < orderList.size(); i++) {
			Item item = itemRepository.findById(itemList.get(i))
				.orElseThrow(ItemNotFoundException::new);
			item.updateOrder(orderList.get(i));
			itemRepository.save(item);
		}
	}

	public void deletePortfolio(Users users, Long portfolioId) {
		Portfolio portfolio = this.findPortfolioById(portfolioId);

		checkPortfolioDeleted(portfolio);

		//작성자가 아닌 user 예외처리
		if (!portfolio.getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}

		portfolio.deletePortfolio();

		portfolio.getPortfolioItemList().forEach(item -> {
			item.deleteItem();
			itemRepository.save(item);
		});

		portfolioRepository.save(portfolio);
	}

	public Portfolio findPortfolioById(Long id) {
		return portfolioRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);
	}

	public Portfolio checkPortfolioDeleted(Portfolio portfolio) {
		if (Boolean.TRUE.equals(portfolio.getIsDeleted())) {
			throw new PortfolioNotFoundException();
		}
		return portfolio;
	}
}
