package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileUpdateResDto {
	private String nickname;
	private PlannerInfoDto plannerInfo;
	private PlannerProfileInfoDto plannerProfileInfo;

	@Builder
	public PlannerProfileUpdateResDto(String nickname,
		PlannerInfoDto plannerInfo, PlannerProfileInfoDto plannerProfileInfo) {
		this.nickname = nickname;
		this.plannerInfo = plannerInfo;
		this.plannerProfileInfo = plannerProfileInfo;
	}
}
