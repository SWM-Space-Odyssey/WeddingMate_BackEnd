package swmaestro.spaceodyssey.weddingmate.domain.oauth2;

import java.util.Map;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.OAUTH_AGE_NOT_PROVIDED;
import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.OAUTH_GENDER_NOT_PROVIDED;

public class KakaoOAuth2User extends OAuth2UserInfo {

	private Long oauth2Id;

	public KakaoOAuth2User(Map<String, Object> attributes) {
		super((Map<String, Object>) attributes.get("kakao_account"));
		this.oauth2Id = (Long) attributes.get("id");
	}

	@Override
	public String getOAuth2Id() {
		return this.oauth2Id.toString();
	}

	@Override
	public String getName() {
		return (String) ((Map<String, Object>) attributes.get("profile")).get("nickname");
	}

	@Override
	public String getEmail() {
		return (String) attributes.get("email");
	}

	@Override
	public String getImageUrl() {
		return (String) ((Map<String, Object>) attributes.get("profile")).get("thumbnail_image_url");
	}

	@Override
	public String getGender() {
		String gender = (String) attributes.get("gender");
		if (gender == null) {
			gender = OAUTH_GENDER_NOT_PROVIDED;
		}
		return gender;
	}

	@Override
	public String getAge() {
		String age = (String) attributes.get("age_range");
		if (age == null) {
			age = OAUTH_AGE_NOT_PROVIDED;
		}
		return age;
	}
}
