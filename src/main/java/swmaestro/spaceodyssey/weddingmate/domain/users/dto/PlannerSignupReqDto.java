package swmaestro.spaceodyssey.weddingmate.domain.users.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class PlannerSignupReqDto {
	String nickname;
	String company;
	String position;
	String region;
	List<String> plannerTagList;
}
