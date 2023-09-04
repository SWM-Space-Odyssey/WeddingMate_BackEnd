package swmaestro.spaceodyssey.weddingmate.domain.file.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FeedResDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileMapper;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.ImageListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FileRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class FileService {

	private final FileRepository fileRepository;
	private final FileMapper fileMapper;

	public FeedResDto getImagesAfterCursor(Long cursor, int pageSize) {
		List<File> fileList = fileRepository.findFilesAfterCursor(cursor, pageSize);

		File lastFile = fileList.get(fileList.size() - 1);

		List<ImageListResDto> imageListResDtoList = new ArrayList<>(fileList.stream().map(fileMapper::entityToDto).toList());

		//이미지 랜덤하게 섞기
		Collections.shuffle(imageListResDtoList);

		if (fileList.size() != pageSize) {
			return fileMapper.createFeedResponse(imageListResDtoList, Long.valueOf(0), fileList.size());
		}
		return fileMapper.createFeedResponse(imageListResDtoList, lastFile.getFileId(), fileList.size());
	}

	public FeedResDto getFirstPageImages(int pageSize) {
		Long cursor = fileRepository.findMaxFileId();
		Long randomCursor = getRandomCursor(cursor);
		return getImagesAfterCursor(randomCursor, pageSize);
	}

	public Long getRandomCursor(Long max) {
		Random random = new Random();
		return Math.abs(random.nextLong()) % max;
	}


	public File findById(Long fileId) {
		return fileRepository.findById(fileId)
			.orElseThrow(FileNotFoundException::new);
	}
}
