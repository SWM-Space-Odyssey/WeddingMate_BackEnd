package swmaestro.spaceodyssey.weddingmate.domain.like.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
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
