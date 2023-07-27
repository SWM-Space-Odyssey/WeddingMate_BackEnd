package swmaestro.spaceodyssey.weddingmate.domain.users.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class CustomerSignupReqDto {
	String nickname;
	String weddingDate;
	String budget;
	List<String> portfolioTagList;
	List<String> plannerTagList;
}
