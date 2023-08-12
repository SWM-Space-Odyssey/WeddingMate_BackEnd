package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class ItemOrderDto {
	private Long itemId;
	private int itemOrder;
}
