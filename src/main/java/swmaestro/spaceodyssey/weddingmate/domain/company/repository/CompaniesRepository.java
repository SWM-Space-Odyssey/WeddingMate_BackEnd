package swmaestro.spaceodyssey.weddingmate.domain.company.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;

public interface CompaniesRepository extends JpaRepository<Companies, Long> {

	@Query(
		value = "SELECT * FROM companies " +
			"WHERE MATCH(name, category) " +
			"AGAINST(:keyword IN NATURAL LANGUAGE MODE)",
		nativeQuery = true
	)
	List<Companies> searchByFullText(@Param("keyword") String keyword);
}
