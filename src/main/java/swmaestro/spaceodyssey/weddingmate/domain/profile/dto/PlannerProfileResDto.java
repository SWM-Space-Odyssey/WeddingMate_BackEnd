package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileResDto {
	private Long plannerProfileId;
	private ProfileImageUrlDto profileImageUrlDto;
	private String bio;
	private String sns;

	@Builder
	public PlannerProfileResDto(Long plannerProfileId, ProfileImageUrlDto profileImageUrlDto, String bio, String sns) {
		this.plannerProfileId = plannerProfileId;
		this.profileImageUrlDto = profileImageUrlDto;
		this.bio = bio;
		this.sns = sns;
	}
}
