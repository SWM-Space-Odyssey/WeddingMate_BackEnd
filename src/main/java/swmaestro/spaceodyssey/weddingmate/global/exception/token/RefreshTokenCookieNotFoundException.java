package swmaestro.spaceodyssey.weddingmate.global.exception.token;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class RefreshTokenCookieNotFoundException extends RuntimeException {
	public RefreshTokenCookieNotFoundException() {
		super(REFRESH_TOKEN_COOKIE_NOTFOUND);
	}
}
