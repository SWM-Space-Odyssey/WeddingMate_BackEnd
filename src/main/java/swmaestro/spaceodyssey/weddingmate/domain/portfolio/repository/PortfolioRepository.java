package swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;

public interface PortfolioRepository extends JpaRepository<Portfolio, Long> {

}
