package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import java.util.List;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemOrderDto;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfolioUpdateReqDto {
	private String title;
	private String region;
	private String tags;
	private List<ItemOrderDto> itemOrderList;

	@Builder
	public PortfolioUpdateReqDto(String title, String region, String tags, List<ItemOrderDto> itemOrderList) {
		this.title = title;
		this.region = region;
		this.tags = tags;
		this.itemOrderList = itemOrderList;
	}
}
