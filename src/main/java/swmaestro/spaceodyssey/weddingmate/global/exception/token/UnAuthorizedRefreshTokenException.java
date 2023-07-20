package swmaestro.spaceodyssey.weddingmate.global.exception.token;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.UNAUTHORIZED)
public class UnAuthorizedRefreshTokenException extends RuntimeException{

	public UnAuthorizedRefreshTokenException() {
		super(REFRESH_TOKEN_UNAUTHORIZED);
	}
}
