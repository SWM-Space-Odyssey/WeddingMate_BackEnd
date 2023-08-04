package swmaestro.spaceodyssey.weddingmate.global.exception.file;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
public class FileUploadFailureException extends RuntimeException{
	public FileUploadFailureException() {
		super(FILE_UPLOAD_FAILURE);
	}

	public FileUploadFailureException(String message) {
		super(FILE_UPLOAD_FAILURE + message);
	}
}
