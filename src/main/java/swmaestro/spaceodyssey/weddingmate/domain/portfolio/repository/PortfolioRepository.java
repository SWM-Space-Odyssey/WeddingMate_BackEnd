package swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;

public interface PortfolioRepository extends JpaRepository<Portfolio, Long> {

	Optional<Portfolio> findByFile(File file);
}
