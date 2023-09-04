package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ImageListResDto {
	private Long itemId;
	private Long fileId;
	private String url;

	@Builder
	public ImageListResDto(String url, Long fileId, Long itemId) {
		this.url = url;
		this.fileId = fileId;
		this.itemId = itemId;
	}
}
