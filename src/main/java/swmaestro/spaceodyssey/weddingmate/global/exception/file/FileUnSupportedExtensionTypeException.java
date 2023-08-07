package swmaestro.spaceodyssey.weddingmate.global.exception.file;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
public class FileUnSupportedExtensionTypeException extends RuntimeException {
	public FileUnSupportedExtensionTypeException() {
		super(UNSUPPORTED_FILE_EXTENSION);
	}
}
