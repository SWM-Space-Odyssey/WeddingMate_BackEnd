package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerProfileUpdateReqDto {
	private String bio;
	private String sns;
	private String tagList;
}
