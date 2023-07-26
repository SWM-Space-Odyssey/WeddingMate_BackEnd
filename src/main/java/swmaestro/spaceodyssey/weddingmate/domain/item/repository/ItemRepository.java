package swmaestro.spaceodyssey.weddingmate.domain.item.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;

public interface ItemRepository extends JpaRepository<Item, Long> {

}