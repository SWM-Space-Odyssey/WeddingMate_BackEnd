package swmaestro.spaceodyssey.weddingmate.domain.item.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;

public interface ItemsRepository extends JpaRepository<Items, Long> {
}
