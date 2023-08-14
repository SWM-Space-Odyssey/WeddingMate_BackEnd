package swmaestro.spaceodyssey.weddingmate.domain.users.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class PlannerSignupReqDto {
	private String nickname;

	private String company;
	private String position;
	private String region;
	private String plannerTagList;
}
