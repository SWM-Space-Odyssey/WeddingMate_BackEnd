package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileInfoResDto {
	private Long plannerProfileId;
	private String bio;
	private String sns;

	@Builder
	PlannerProfileInfoResDto(Long plannerProfileId, String bio, String sns) {
		this.plannerProfileId = plannerProfileId;
		this.bio = bio;
		this.sns = sns;
	}
}
