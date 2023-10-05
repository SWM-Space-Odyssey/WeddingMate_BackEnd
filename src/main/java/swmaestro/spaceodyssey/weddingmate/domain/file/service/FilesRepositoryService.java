package swmaestro.spaceodyssey.weddingmate.domain.file.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class FilesRepositoryService {

	private final FilesRepository filesRepository;

	@Transactional
	public Files findById(Long fileId) {
		return filesRepository.findById(fileId)
			.orElseThrow(FileNotFoundException::new);
	}

	@Transactional
	public Files findFileByUrl(String url) {
		return filesRepository.findByUrl(url)
			.orElseThrow(FileNotFoundException::new);
	}
}
