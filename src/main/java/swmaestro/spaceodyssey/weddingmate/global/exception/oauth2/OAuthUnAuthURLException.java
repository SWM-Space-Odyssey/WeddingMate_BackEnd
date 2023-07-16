package swmaestro.spaceodyssey.weddingmate.global.exception.oauth2;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.BAD_REQUEST)
public class OAuthUnAuthURLException extends RuntimeException {

	public OAuthUnAuthURLException() {
		super(OAUTH_UNAUTHORIZED_URL);
	}
}
