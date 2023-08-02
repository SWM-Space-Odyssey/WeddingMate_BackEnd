package swmaestro.spaceodyssey.weddingmate.global.exception.oauth2;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;

@ResponseStatus(HttpStatus.CONFLICT)
public class OAuth2DuplicateEmailException extends RuntimeException {
	public OAuth2DuplicateEmailException(AuthProvider authProvider) {
		super(authProvider + OAUTH_DUPLICATE_EMAIL);
	}
}
