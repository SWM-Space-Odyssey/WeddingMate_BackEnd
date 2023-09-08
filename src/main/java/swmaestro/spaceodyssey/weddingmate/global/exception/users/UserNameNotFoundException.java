package swmaestro.spaceodyssey.weddingmate.global.exception.users;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.data.rest.webmvc.ResourceNotFoundException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class UserNameNotFoundException extends ResourceNotFoundException {
	public UserNameNotFoundException() {
		super(USER_NAME_NOTFOUND);
	}
}
