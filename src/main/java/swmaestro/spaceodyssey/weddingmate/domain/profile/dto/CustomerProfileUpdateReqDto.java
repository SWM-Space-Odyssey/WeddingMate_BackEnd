package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerTagListDto;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class CustomerProfileUpdateReqDto {
	private String nickname;

	private CustomerTagListDto customerTagList;
	private CustomerInfoDto customerInfo;
}
