package swmaestro.spaceodyssey.weddingmate.global.exception.file;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.BAD_REQUEST)
public class FileNameEmptyException extends RuntimeException {
	public FileNameEmptyException() {
		super(EMPTY_FILE_NAME);
	}
}
