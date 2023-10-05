package swmaestro.spaceodyssey.weddingmate.domain.file.service;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FeedResDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileMapper;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.ImageListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;

@Service
@RequiredArgsConstructor
@Transactional
public class FilesService {

	private final FilesRepository fileRepository;
	private final FileMapper fileMapper;

	public FeedResDto getImagesAfterCursor(Long cursor, int pageSize) {
		List<Files> fileList = fileRepository.findFilesAfterCursor(cursor, pageSize);

		List<ImageListResDto> imageListResDtoList = new ArrayList<>(
			fileList.stream().map(fileMapper::entityToDto).toList());

		//이미지 랜덤하게 섞기
		Collections.shuffle(imageListResDtoList);

		if (fileList.size() != pageSize) {
			return fileMapper.createFeedResponse(imageListResDtoList, Long.valueOf(0), fileList.size());
		}
		Files lastFile = fileList.get(fileList.size() - 1);

		return fileMapper.createFeedResponse(imageListResDtoList, lastFile.getFileId(), fileList.size());
	}

	public FeedResDto getFirstPageImages(int pageSize) {
		Long cursor = fileRepository.findMaxFileId();
		Long randomCursor = getRandomCursor(cursor);
		return getImagesAfterCursor(randomCursor, pageSize);
	}

	public Long getRandomCursor(Long max) {
		SecureRandom secureRandom = new SecureRandom();
		long randomValue = secureRandom.nextLong();
		if (randomValue < 0) {
			randomValue = -randomValue; // 음수면 양수로 바꾸어 줍니다.
		}
		return randomValue % max;
	}
}

