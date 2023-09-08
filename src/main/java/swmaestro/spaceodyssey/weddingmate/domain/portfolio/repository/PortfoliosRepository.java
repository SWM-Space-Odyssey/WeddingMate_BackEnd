package swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;

public interface PortfoliosRepository extends JpaRepository<Portfolios, Long> {

	Optional<Portfolios> findByFiles(Files files);
}
