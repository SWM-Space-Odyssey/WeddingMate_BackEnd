package swmaestro.spaceodyssey.weddingmate.global.exception.file;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
public class FileNotImageException extends RuntimeException {
	public FileNotImageException() {
		super(FILE_NOT_IMAGE);
	}

	public FileNotImageException(final String extension) {
		super(FILE_NOT_IMAGE + String.format("[%s]", extension));
	}

	@Override
	public String getMessage() {
		return super.getMessage();
	}
}
