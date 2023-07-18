package swmaestro.spaceodyssey.weddingmate.global.exception.oauth2;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.security.core.AuthenticationException;

import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;

public class OAuth2DuplicateEmailException extends AuthenticationException {
	public OAuth2DuplicateEmailException(AuthProvider authProvider) {
		super(authProvider + OAUTH_DUPLICATE_EMAIL);
	}
}
