package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import java.util.List;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
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
	private Boolean isWriter;

	@Builder
	public ItemResDto(String itemRecord, String company, String date, Long portfolioId, String itemTagList,
		String category, Integer order, Long itemId, List<String> imageList, Boolean isWriter) {
		this.itemRecord = itemRecord;
		this.company = company;
		this.date = date;
		this.portfolioId = portfolioId;
		this.itemTagList = itemTagList;
		this.category = category;
		this.order = order;
		this.itemId = itemId;
		this.imageList = imageList;
		this.isWriter = isWriter;
	}
}
