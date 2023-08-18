package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileResDto {
	private Long plannerProfileId;
	private String nickname;
	private String profileImageUrl;
	private PlannerInfoDto plannerInfo;
	private PlannerProfileInfoDto plannerProfileInfo;

	@Builder
	public PlannerProfileResDto(Long plannerProfileId, String nickname, String profileImageUrl,
		PlannerInfoDto plannerInfo, PlannerProfileInfoDto plannerProfileInfo) {
		this.plannerProfileId = plannerProfileId;
		this.nickname = nickname;
		this.profileImageUrl = profileImageUrl;
		this.plannerInfo = plannerInfo;
		this.plannerProfileInfo = plannerProfileInfo;
	}
}
