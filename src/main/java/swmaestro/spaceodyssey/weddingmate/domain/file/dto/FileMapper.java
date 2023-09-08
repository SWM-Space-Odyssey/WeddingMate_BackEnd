package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import java.util.List;

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

	public ImageListResDto entityToDto(Files file) {

		return ImageListResDto.builder()
			.url(file.getUrl())
			.fileId(file.getFileId())
			.itemId(file.getItems().getItemId())
			.build();
	}

	public FeedResDto createFeedResponse(List<ImageListResDto> imageListResDtoList, Long lastFileId, Integer pageSize) {

		return FeedResDto.builder()
			.nextCursor(lastFileId)
			.pageSize(pageSize)
			.imageListResDtoList(imageListResDtoList)
			.build();
	}

	public Files urlToEntity(String url) {
		return fileRepository.findByUrl(url)
			.orElseThrow(FileNotFoundException::new);
	}
}
