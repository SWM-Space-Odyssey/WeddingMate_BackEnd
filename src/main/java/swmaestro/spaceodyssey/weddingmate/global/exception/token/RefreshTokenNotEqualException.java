package swmaestro.spaceodyssey.weddingmate.global.exception.token;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.data.rest.webmvc.ResourceNotFoundException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class RefreshTokenNotEqualException extends ResourceNotFoundException {
	public RefreshTokenNotEqualException() {
		super(REFRESH_TOKEN_NOT_EQUAL);
	}
}
