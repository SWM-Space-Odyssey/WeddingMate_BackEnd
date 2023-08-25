package swmaestro.spaceodyssey.weddingmate.domain.like.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
public class PlannerLikeResDto {
	private Long id;
	private String name;
	private String profileImg;

	@Builder
	public PlannerLikeResDto(Long id, String name, String profileImg) {
		this.id = id;
		this.name = name;
		this.profileImg = profileImg;
	}
}
