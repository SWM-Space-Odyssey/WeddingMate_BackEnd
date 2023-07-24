package swmaestro.spaceodyssey.weddingmate.domain.category.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;

public interface CategoryRepository extends JpaRepository<Category, Long> {

}
