package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotFoundException;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class FileMapper {
	private final FilesRepository fileRepository;

	public Files urlToEntity(String url) {
		return fileRepository.findByUrl(url)
			.orElseThrow(FileNotFoundException::new);
	}
}
