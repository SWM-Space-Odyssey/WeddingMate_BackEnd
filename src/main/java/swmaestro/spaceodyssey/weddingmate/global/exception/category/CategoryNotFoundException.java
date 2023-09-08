package swmaestro.spaceodyssey.weddingmate.global.exception.category;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.CATEGORY_NOTFOUND;

import org.springframework.data.rest.webmvc.ResourceNotFoundException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class CategoryNotFoundException extends ResourceNotFoundException {
	public CategoryNotFoundException() {
		super(CATEGORY_NOTFOUND);
	}
}

