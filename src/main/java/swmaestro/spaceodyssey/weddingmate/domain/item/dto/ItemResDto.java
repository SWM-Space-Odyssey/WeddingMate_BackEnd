package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import java.util.List;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ItemResDto {
	private final String itemRecord;
	private final String company;
	private final String date;
	private final Long portfolioId;
	private final List<String> itemTagList;
	private final String categoryContent;
	private final Integer order;
	private final Long itemId;
}
