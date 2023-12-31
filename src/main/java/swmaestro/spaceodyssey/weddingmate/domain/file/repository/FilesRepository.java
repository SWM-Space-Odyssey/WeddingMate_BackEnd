package swmaestro.spaceodyssey.weddingmate.domain.file.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;

public interface FilesRepository extends JpaRepository<Files, Long> {
	Optional<Files> findByUrl(String url);

	List<Files> findByItems(Items items);

	@Query(value = "SELECT f.* FROM files f INNER JOIN items i ON f.item_id = i.item_id WHERE f.file_id > :cursor AND i.is_deleted = false LIMIT :limit", nativeQuery = true)
	List<Files> findFilesAfterCursor(@Param("cursor") Long cursor, @Param("limit") int limit);

	@Query(value = "SELECT MAX(fileId) FROM Files")
	Long findMaxFileId();
}
