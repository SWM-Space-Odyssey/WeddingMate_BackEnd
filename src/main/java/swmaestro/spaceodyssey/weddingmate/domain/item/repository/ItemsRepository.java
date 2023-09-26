package swmaestro.spaceodyssey.weddingmate.domain.item.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import io.lettuce.core.dynamic.annotation.Param;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;

public interface ItemsRepository extends JpaRepository<Items, Long> {
	@Query(
		value = "SELECT * FROM items "
			+ "WHERE MATCH(item_tag_list, company_name, category, item_record) "
			+ "AGAINST(:keyword IN NATURAL LANGUAGE MODE) AND is_deleted = false",
		nativeQuery = true)
	List<Items> searchItemsByKeyword(@Param("keyword") String keyword);

}
