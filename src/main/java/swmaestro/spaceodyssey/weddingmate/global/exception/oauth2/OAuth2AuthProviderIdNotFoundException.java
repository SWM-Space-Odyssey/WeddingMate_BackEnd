package swmaestro.spaceodyssey.weddingmate.global.exception.oauth2;

import org.springframework.data.rest.webmvc.ResourceNotFoundException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class OAuth2AuthProviderIdNotFoundException extends ResourceNotFoundException {

	public OAuth2AuthProviderIdNotFoundException() {
		super(OAUTH_NOTFOUND_PROVIDERID);
	}
}
