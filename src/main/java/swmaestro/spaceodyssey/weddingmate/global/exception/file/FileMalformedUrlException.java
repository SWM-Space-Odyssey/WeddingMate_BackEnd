package swmaestro.spaceodyssey.weddingmate.global.exception.file;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.BAD_REQUEST)
public class FileMalformedUrlException extends RuntimeException{
	public FileMalformedUrlException() {
		super(FILE_MALFORMED_URL);
	}

	public FileMalformedUrlException(String message) {
		super(FILE_MALFORMED_URL + message);
	}
}
