package swmaestro.spaceodyssey.weddingmate.domain.item.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemsRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class ItemsRepositoryService {

	private final ItemsRepository itemsRepository;

	@Transactional
	public Items findItemById(Long id) {
		return itemsRepository.findById(id)
			.orElseThrow(ItemNotFoundException::new);
	}

	@Transactional
	public void softDeleteItemsByUserId(Long userId) {
		itemsRepository.softDeleteItemsByUserId(userId);
	}

	@Transactional
	public void softDeleteItemByPortfolioId(Long portfolioId) {
		itemsRepository.softDeleteItemByPortfolioId(portfolioId);
	}
}