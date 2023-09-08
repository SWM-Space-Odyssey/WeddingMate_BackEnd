package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemSearchResDto {
	private Long itemId;
	private String url;

	@Builder
	public ItemSearchResDto(String url, Long itemId) {
		this.url = url;
		this.itemId = itemId;
	}
}
