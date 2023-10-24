package swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface PortfoliosRepository extends JpaRepository<Portfolios, Long> {

	Optional<Portfolios> findByFiles(Files files);

	List<Portfolios> findAllByUsers(Users users);

	@Modifying
	@Query("UPDATE Portfolios p SET p.isDeleted = true WHERE p.users = :user")
	void softDeleteByUser(@Param("user") Users user);
}
