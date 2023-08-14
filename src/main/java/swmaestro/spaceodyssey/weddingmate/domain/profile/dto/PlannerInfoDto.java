package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerInfoDto {
	private String company;
	private String position;
	private String region;
	private String plannerTagList;

	@Builder
	public PlannerInfoDto(String company, String position, String region, String plannerTagList) {
		this.company = company;
		this.position = position;
		this.region = region;
		this.plannerTagList = plannerTagList;
	}
}