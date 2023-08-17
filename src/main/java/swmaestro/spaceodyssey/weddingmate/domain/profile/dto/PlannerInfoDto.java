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
	private String tagList;

	@Builder
	public PlannerInfoDto(String company, String position, String region, String tagList) {
		this.company = company;
		this.position = position;
		this.region = region;
		this.tagList = tagList;
	}
}
