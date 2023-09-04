package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FileRepository;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class FileMapper {
	private final FileRepository fileRepository;

	public ImageListResDto entityToDto(File file) {

		return ImageListResDto.builder()
			.url(file.getUrl())
			.fileId(file.getFileId())
			.itemId(file.getItem().getItemId())
			.build();
	}

	public FeedResDto createFeedResponse(List<ImageListResDto> imageListResDtoList, Long lastFileId, Integer pageSize) {

		return FeedResDto.builder()
			.nextCursor(lastFileId)
			.pageSize(pageSize)
			.imageListResDtoList(imageListResDtoList)
			.build();
	}
}
