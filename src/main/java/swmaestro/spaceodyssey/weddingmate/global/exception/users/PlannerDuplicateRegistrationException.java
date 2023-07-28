package swmaestro.spaceodyssey.weddingmate.global.exception.users;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.CONFLICT)
public class PlannerDuplicateRegistrationException extends RuntimeException {
	public PlannerDuplicateRegistrationException() {
		super(PLANNER_DUPLICATE_REGISTRATION);
	}
}
