package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemMapper;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.PortfolioTag;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagMapper;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagResDto;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class PortfolioService {
	private final PortfolioRepository portfolioRepository;
	private final UsersRepository usersRepository;
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
			Tag tag = this.tagMapper.contentToEntity(portfolioTag);
			tagList.add(new PortfolioTag(portfolio, tag));
		});

		portfolio.setPortfolioTag(tagList);
		portfolioRepository.save(portfolio);
	}

	public PortfolioDetailResDto findById(Long id) {
		Portfolio portfolio = portfolioRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);

		List<TagResDto> tagList = portfolio.getPortfolioTagList().stream()
			.map(PortfolioTag::getTag)
			.map(tagMapper::entityToDto)
			.toList();

		List<ItemResDto> itemResDtoList = portfolio.getPortfolioItemList().stream()
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
			.map(portfolioMapper::entityToDto)
			.toList();
	}
}
