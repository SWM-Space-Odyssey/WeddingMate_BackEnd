package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ItemSearchResDto {
	Long itemId;
	String url;

	@Builder
	public ItemSearchResDto(String url, Long itemId) {
		this.url = url;
		this.itemId = itemId;
	}
}
