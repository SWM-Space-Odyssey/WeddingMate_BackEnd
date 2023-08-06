package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileUpdateResDto {
	private String bio;
	private String sns;

	@Builder
	public PlannerProfileUpdateResDto(String bio, String sns){
		this.bio = bio;
		this.sns = sns;
	}
}
