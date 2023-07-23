package swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;

@Repository
public interface PortfolioRepository extends JpaRepository<Portfolio, Long> {

}
