package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemOrderDto;

@Getter
@AllArgsConstructor
public class PortfolioUpdateReqDto {
	private String title;
	private String region;
	private String tags;
	private List<ItemOrderDto> itemOrderList;
}
