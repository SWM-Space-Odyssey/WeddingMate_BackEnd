package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ImageListResDto {
	Long itemId;
	Long portfolioId;
	String url;

	@Builder
	public ImageListResDto(String url, Long itemId, Long portfolioId) {
		this.url = url;
		this.itemId = itemId;
		this.portfolioId = portfolioId;
	}
}
