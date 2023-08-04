package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ProfileImageUrlDto {
	private String profileImageUrl;

	@Builder
	public ProfileImageUrlDto(String profileImageUrl){
		this.profileImageUrl = profileImageUrl;
	}
}
