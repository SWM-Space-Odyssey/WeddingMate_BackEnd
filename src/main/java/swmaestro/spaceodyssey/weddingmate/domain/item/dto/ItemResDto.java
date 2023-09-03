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
	private Boolean isLiked;

	@Builder
	public ItemResDto(Long portfolioId, Integer order, Long itemId, List<String> imageList, Boolean isWriter, ItemDetail itemDetail, Boolean isLiked) {
		this.itemRecord = itemDetail.itemRecord;
		this.company = itemDetail.company;
		this.date = itemDetail.date;
		this.itemTagList = itemDetail.itemTagList;
		this.category = itemDetail.category;
		this.portfolioId = portfolioId;
		this.order = order;
		this.itemId = itemId;
		this.imageList = imageList;
		this.isWriter = isWriter;
		this.isLiked = isLiked;
	}

	@Builder
	public static class ItemDetail {
		private String itemRecord;
		private String company;
		private String date;
		private String itemTagList;
		private String category;
	}
}
