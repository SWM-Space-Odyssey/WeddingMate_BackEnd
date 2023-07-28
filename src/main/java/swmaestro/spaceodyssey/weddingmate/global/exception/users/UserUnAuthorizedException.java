package swmaestro.spaceodyssey.weddingmate.global.exception.users;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.UNAUTHORIZED)
public class UserUnAuthorizedException extends RuntimeException {
	public UserUnAuthorizedException() {
		super(USER_UNAUTHORIZED);
	}
}

