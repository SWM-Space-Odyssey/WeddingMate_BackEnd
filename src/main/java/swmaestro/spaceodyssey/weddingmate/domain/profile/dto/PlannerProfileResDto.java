package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileResDto {
	private String nickname;
	private String profileImageUrl;
	private PlannerInfoDto plannerInfo;
	private PlannerProfileInfoResDto plannerProfileInfo;

	@Builder
	public PlannerProfileResDto(String nickname, String profileImageUrl, PlannerInfoDto plannerInfo, PlannerProfileInfoResDto plannerProfileInfo) {
		this.nickname = nickname;
		this.profileImageUrl = profileImageUrl;
		this.plannerInfo = plannerInfo;
		this.plannerProfileInfo = plannerProfileInfo;
	}
}
