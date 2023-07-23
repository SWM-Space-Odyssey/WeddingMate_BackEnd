package swmaestro.spaceodyssey.weddingmate.domain.tag.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;

@Repository
public interface TagRepository extends JpaRepository<Tag, Long> {
	Optional<Tag> findByContent(String content);
}
