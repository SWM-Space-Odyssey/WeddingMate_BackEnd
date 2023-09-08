package swmaestro.spaceodyssey.weddingmate.domain.oauth2;

import java.util.Map;

public class KakaoOAuth2User extends OAuth2UserInfo {

	private Long oauth2Id;

	public KakaoOAuth2User(Map<String, Object> attributes) {
		super((Map<String, Object>)attributes.get("kakao_account"));
		this.oauth2Id = (Long)attributes.get("id");
	}

	@Override
	public String getOAuth2Id() {
		return this.oauth2Id.toString();
	}

	@Override
	public String getName() {
		return (String)((Map<String, Object>)attributes.get("profile")).get("nickname");
	}

	@Override
	public String getEmail() {
		return (String)attributes.get("email");
	}

	@Override
	public String getImageUrl() {
		return (String)((Map<String, Object>)attributes.get("profile")).get("thumbnail_image_url");
	}
}
