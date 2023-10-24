package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import java.util.List;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfolioDetailResDto {
	private Long id;
	private String title;
	private String tagList;
	private String region;
	private List<ItemResDto> itemResDtoList;
	private String repImgUrl;
	private Boolean isWriter;
	private Long userId;
	private Boolean isLiked;

	@Builder
	public PortfolioDetailResDto(Long id, String title, String tagList, List<ItemResDto> itemResDtoList,
		String repImgUrl,
		Boolean isWriter, String region, Long userId, Boolean isLiked) {
		this.id = id;
		this.title = title;
		this.tagList = tagList;
		this.itemResDtoList = itemResDtoList;
		this.repImgUrl = repImgUrl;
		this.isWriter = isWriter;
		this.region = region;
		this.userId = userId;
		this.isLiked = isLiked;
	}
}
