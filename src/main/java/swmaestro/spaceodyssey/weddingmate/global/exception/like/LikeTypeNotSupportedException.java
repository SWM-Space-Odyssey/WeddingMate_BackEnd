package swmaestro.spaceodyssey.weddingmate.global.exception.like;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.BAD_REQUEST)
public class LikeTypeNotSupportedException extends RuntimeException {
	public LikeTypeNotSupportedException() {
		super(LIKE_TYPE_NOT_SUPPORTED);
	}
}



