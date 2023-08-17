package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileInfoDto {
	private String bio;
	private String sns;

	@Builder
	PlannerProfileInfoDto(String bio, String sns) {
		this.bio = bio;
		this.sns = sns;
	}
}
