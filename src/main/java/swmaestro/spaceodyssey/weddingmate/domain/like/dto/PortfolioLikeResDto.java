package swmaestro.spaceodyssey.weddingmate.domain.like.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfolioLikeResDto {
	private Long id;
	private String title;
	private String repImgUrl;

	@Builder
	public PortfolioLikeResDto(Long id, String title, String repImgUrl) {
		this.id = id;
		this.title = title;
		this.repImgUrl = repImgUrl;
	}
}
