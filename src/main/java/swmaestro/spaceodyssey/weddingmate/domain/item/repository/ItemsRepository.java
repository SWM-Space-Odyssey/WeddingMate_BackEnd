package swmaestro.spaceodyssey.weddingmate.domain.item.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;

public interface ItemsRepository extends JpaRepository<Items, Long> {
	@Query(
		value = "SELECT * FROM items "
			+ "WHERE MATCH(item_tag_list, company_name, category, item_record) "
			+ "AGAINST(:keyword IN NATURAL LANGUAGE MODE) AND is_deleted = false",
		nativeQuery = true)
	List<Items> searchItemsByKeyword(@Param("keyword") String keyword);

	@Modifying
	@Query("UPDATE Items item SET item.isDeleted = true WHERE item IN (SELECT item FROM Portfolios p JOIN p.users u WHERE u.userId = :userId)")
	void softDeleteItemsByUserId(@Param("userId") Long userId);

	@Modifying
	@Query("UPDATE Items item SET item.isDeleted = true WHERE item.portfolios.portfolioId = :portfolioId")
	void softDeleteItemByPortfolioId(@Param("portfolioId") Long portfolioId);
}
