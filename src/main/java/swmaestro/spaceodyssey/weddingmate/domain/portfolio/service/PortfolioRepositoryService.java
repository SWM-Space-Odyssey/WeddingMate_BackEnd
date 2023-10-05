package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class PortfolioRepositoryService {

	private final PortfoliosRepository portfoliosRepository;

	@Transactional
	public Portfolios findPortfolioById(Long id) {
		return portfoliosRepository.findById(id)
			.orElseThrow(PortfolioNotFoundException::new);
	}

	@Transactional
	public void softDeletePortfolioByUsers(Users users) {
		portfoliosRepository.softDeleteByUser(users);
	}
}
