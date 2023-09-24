package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerTagListDto;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class CustomerProfileResDto {
	private Long userId;
	private String nickname;
	private String profileImageUrl;
	private CustomerTagListDto customerTagListDto;
	private CustomerInfoDto customerInfoDto;

	@Builder
	public CustomerProfileResDto(Long userId, String nickname, String profileImageUrl, CustomerTagListDto customerTagListDto, CustomerInfoDto customerInfoDto) {
		this.userId = userId;
		this.nickname = nickname;
		this.profileImageUrl = profileImageUrl;
		this.customerTagListDto = customerTagListDto;
		this.customerInfoDto = customerInfoDto;
	}
}
