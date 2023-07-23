package swmaestro.spaceodyssey.weddingmate.domain.category.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;

@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

}
