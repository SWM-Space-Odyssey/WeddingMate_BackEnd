package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import java.util.List;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class FeedResDto {
	private Long nextCursor;
	private Integer pageSize;
	private List<ImageListResDto> imageListResDtoList;

	@Builder
	public FeedResDto(Long nextCursor, Integer pageSize, List<ImageListResDto> imageListResDtoList) {
		this.nextCursor = nextCursor;
		this.pageSize = pageSize;
		this.imageListResDtoList = imageListResDtoList;
	}
}
