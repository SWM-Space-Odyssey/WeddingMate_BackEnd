package swmaestro.spaceodyssey.weddingmate.domain.file.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import io.lettuce.core.dynamic.annotation.Param;

public interface FilesRepository extends JpaRepository<Files, Long> {
	Optional<Files> findByUrl(String url);

	List<Files> findByItems(Items items);

	@Query(value = "SELECT * FROM files WHERE file_id > :cursor AND item_id IS NOT NULL ORDER BY file_id ASC LIMIT :limit", nativeQuery = true)
	List<Files> findFilesAfterCursor(@Param("cursor") Long cursor, @Param("limit") int limit);

	@Query(value = "SELECT MAX(fileId) FROM Files")
	Long findMaxFileId();
}
