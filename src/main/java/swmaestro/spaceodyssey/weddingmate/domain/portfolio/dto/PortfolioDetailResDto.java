package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import java.util.List;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagResDto;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfolioDetailResDto {
	private Long id;
	private String title;
	private List<TagResDto> tagResDtoList;
	private List<ItemResDto> itemResDtoList;

	@Builder
	public PortfolioDetailResDto(Long id, String title, List<TagResDto> tagResDtoList, List<ItemResDto> itemResDtoList) {
		this.id = id;
		this.title = title;
		this.tagResDtoList = tagResDtoList;
		this.itemResDtoList = itemResDtoList;
	}
}
