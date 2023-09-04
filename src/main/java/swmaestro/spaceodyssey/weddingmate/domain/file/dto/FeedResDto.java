package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import java.util.List;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class FeedResDto {
	Long nextCursor;
	Integer pageSize;
	List<ImageListResDto> imageListResDtoList;

	@Builder
	public FeedResDto(Long nextCursor, Integer pageSize, List<ImageListResDto> imageListResDtoList) {
		this.nextCursor = nextCursor;
		this.pageSize = pageSize;
		this.imageListResDtoList = imageListResDtoList;
	}
}
