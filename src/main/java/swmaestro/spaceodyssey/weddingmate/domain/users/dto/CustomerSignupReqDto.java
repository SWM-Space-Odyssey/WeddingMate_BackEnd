package swmaestro.spaceodyssey.weddingmate.domain.users.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class CustomerSignupReqDto {
	// User 관련 속성
	private String nickname;

	// Customer 관련 속성
	private Boolean weddingDateConfirmed;
	private String weddingDate;
	private String region;
	private String budget;

	// CustomerTagList 관련 속성
	private CustomerTagListDto customerTagList;
}
