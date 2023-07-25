package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class PortfolioUpdateReqDto {
	private String title;
	private List<String> tags;
	private List<Long> itemList;
	private List<Integer> orderList;
}
