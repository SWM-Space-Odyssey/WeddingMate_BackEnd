package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

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
	private Long categoryId;
	private Integer order;
}
