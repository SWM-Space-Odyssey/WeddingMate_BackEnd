package swmaestro.spaceodyssey.weddingmate.domain.item.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import io.lettuce.core.dynamic.annotation.Param;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;

public interface ItemRepository extends JpaRepository<Item, Long> {

	@Query(value = "SELECT * FROM item WHERE MATCH(item_tag_list, company, category) AGAINST(:keyword IN BOOLEAN MODE) AND is_deleted = false", nativeQuery = true)
	List<Item> searchItemsByFullText(@Param("keyword") String keyword);
}
