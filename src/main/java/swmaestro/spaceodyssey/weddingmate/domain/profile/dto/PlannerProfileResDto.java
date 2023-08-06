package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileResDto {
	private Long plannerProfileId;
	private String profileImageUrl;
	private String bio;
	private String sns;

	@Builder
	public PlannerProfileResDto(Long plannerProfileId, String profileImageUrl, String bio, String sns) {
		this.plannerProfileId = plannerProfileId;
		this.profileImageUrl = profileImageUrl;
		this.bio = bio;
		this.sns = sns;
	}
}
