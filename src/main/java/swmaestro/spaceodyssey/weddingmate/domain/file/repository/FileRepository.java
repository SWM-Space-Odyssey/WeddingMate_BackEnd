package swmaestro.spaceodyssey.weddingmate.domain.file.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;

public interface FileRepository extends JpaRepository<File, Long> {
	Optional<File> findByUrl(String url);

	List<File> findByItem(Item item);
}
