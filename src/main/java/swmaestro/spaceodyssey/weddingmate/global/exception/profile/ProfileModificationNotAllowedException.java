package swmaestro.spaceodyssey.weddingmate.global.exception.profile;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.FORBIDDEN)
public class ProfileModificationNotAllowedException extends RuntimeException {
	public ProfileModificationNotAllowedException() { super(PROFILE_MODIFICATION_NOT_ALLOWED); }
}
