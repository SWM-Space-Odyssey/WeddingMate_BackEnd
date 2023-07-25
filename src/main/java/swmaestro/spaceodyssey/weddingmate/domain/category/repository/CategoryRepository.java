package swmaestro.spaceodyssey.weddingmate.domain.category.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;

public interface CategoryRepository extends JpaRepository<Category, Long> {
	List<Category> findByIsDefault(Boolean isDefault);

	Optional<Category> findByContent(String content);
}
