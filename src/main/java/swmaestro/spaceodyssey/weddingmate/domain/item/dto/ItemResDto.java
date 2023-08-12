package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class ItemResDto {
	private String itemRecord;
	private String company;
	private String date;
	private Long portfolioId;
	private String itemTagList;
	private String category;
	private Integer order;
	private Long itemId;
	private List<String> imageList;
}
