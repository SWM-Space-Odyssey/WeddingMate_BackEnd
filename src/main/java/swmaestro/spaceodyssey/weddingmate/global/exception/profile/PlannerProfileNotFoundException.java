package swmaestro.spaceodyssey.weddingmate.global.exception.profile;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.data.rest.webmvc.ResourceNotFoundException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class PlannerProfileNotFoundException extends ResourceNotFoundException {
	public PlannerProfileNotFoundException() {
		super(PLANNER_PROFILE_NOTFOUND);
	}
}
