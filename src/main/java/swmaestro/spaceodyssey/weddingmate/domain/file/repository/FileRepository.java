package swmaestro.spaceodyssey.weddingmate.domain.file.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import io.lettuce.core.dynamic.annotation.Param;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;

public interface FileRepository extends JpaRepository<File, Long> {
	Optional<File> findByUrl(String url);

	List<File> findByItem(Item item);

	@Query(value = "SELECT * FROM file WHERE file_id > :cursor AND item_id IS NOT NULL ORDER BY file_id ASC LIMIT :limit", nativeQuery = true)
	List<File> findFilesAfterCursor(@Param("cursor") Long cursor, @Param("limit") int limit);

	@Query(value = "SELECT MAX(fileId) FROM File")
	Long findMaxFileId();
}
