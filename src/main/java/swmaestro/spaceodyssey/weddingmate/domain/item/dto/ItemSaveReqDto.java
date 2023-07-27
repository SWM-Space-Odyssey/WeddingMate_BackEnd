package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class ItemSaveReqDto {
	private String itemRecord;
	private String company;
	private String date;
	private Long portfolioId;
	private List<String> itemTagList;
	private String categoryContent;
	private Integer order;
	private List<String> imageList;
}
