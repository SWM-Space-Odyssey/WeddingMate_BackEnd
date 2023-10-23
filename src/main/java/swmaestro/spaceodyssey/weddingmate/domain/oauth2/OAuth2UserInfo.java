package swmaestro.spaceodyssey.weddingmate.domain.oauth2;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public abstract class OAuth2UserInfo {

	protected Map<String, Object> attributes;

	public Map<String, Object> getAttributes() {
		return attributes;
	}

	public abstract String getOAuth2Id();

	public abstract String getName();

	public abstract String getEmail();

	public abstract String getImageUrl();
}
