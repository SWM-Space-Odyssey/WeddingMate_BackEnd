package swmaestro.spaceodyssey.weddingmate.global.exception.handler;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;

import static org.springframework.http.HttpStatus.BAD_REQUEST;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {
	/* Common */
	//Basic - C0***
	RUNTIME_EXCEPTION(BAD_REQUEST, "C0001", "RUNTIME_EXCEPTION");

	private final HttpStatus status;
	private final String code;
	private final String message;
}

