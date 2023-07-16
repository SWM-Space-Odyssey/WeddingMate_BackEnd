package swmaestro.spaceodyssey.weddingmate.domain.oauth2;

import java.util.Map;
import java.util.Objects;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class OAuth2UserInfoFactory {
	public static OAuth2UserInfo getOAuth2UserInfo(AuthProvider authProvider, Map<String, Object> attributes) {
		if (Objects.requireNonNull(authProvider) == AuthProvider.KAKAO) {
			return new KakaoOAuth2User(attributes);
		}
		throw new IllegalArgumentException("Invalid Provider Type.");
	}
}
