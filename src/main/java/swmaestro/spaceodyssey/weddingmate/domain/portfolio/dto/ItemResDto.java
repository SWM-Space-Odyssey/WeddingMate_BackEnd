package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import java.util.List;

import lombok.Builder;
import lombok.Getter;

@Getter
public class ItemResDto {
	private String itemRecord;
	private String company;
	private String date;
	private Long portfolioId;
	private List<String> itemTagList;
	private Long categoryId;
	private Integer order;

	@Builder
	public ItemResDto(String itemRecord, String company, String date,
							Long portfolioId, List<String> itemTagList, Long categoryId, Integer order) {
		this.itemRecord = itemRecord;
		this.company = company;
		this.date = date;
		this.portfolioId = portfolioId;
		this.itemTagList = itemTagList;
		this.categoryId = categoryId;
		this.order = order;
	}
}
