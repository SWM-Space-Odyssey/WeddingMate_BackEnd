package swmaestro.spaceodyssey.weddingmate.global.exception.company;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.data.rest.webmvc.ResourceNotFoundException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class CompanyNotFoundException extends ResourceNotFoundException {
	public CompanyNotFoundException() {
		super((COMPANY_NOTFOUND));
	}
}
