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
	private String companyName;
	private Long companyId;
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
	public ItemResDto(Long portfolioId, Integer order, Long itemId, List<String> imageList, Boolean isWriter, ItemDetail itemDetail, Boolean isLiked, ItemCompany itemCompany) {
		this.itemRecord = itemDetail.itemRecord;
		this.date = itemDetail.date;
		this.itemTagList = itemDetail.itemTagList;
		this.category = itemDetail.category;
		this.companyName = itemCompany.companyName;
		this.companyId = itemCompany.companyId;
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
		private String date;
		private String itemTagList;
		private String category;
	}

	@Builder
	public static class ItemCompany {
		private Long companyId;
		private String companyName;
	}
}
