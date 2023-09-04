package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ImageListResDto {
	Long itemId;
	Long fileId;
	String url;

	@Builder
	public ImageListResDto(String url, Long fileId, Long itemId) {
		this.url = url;
		this.fileId = fileId;
		this.itemId = itemId;
	}
}
