package swmaestro.spaceodyssey.weddingmate.domain.file.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;

public interface FilesRepository extends JpaRepository<Files, Long> {
	Optional<Files> findByUrl(String url);

	List<Files> findByItems(Items items);
}
