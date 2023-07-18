package swmaestro.spaceodyssey.weddingmate.global.exception.handler;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import lombok.Builder;
import lombok.Getter;

@Getter
public class ErrorResponse {
	private HttpStatus status;
	private static final Boolean success = false;
	private ErrorCode code;
	private String message;

	@Builder
	public ErrorResponse(HttpStatus status, ErrorCode code, String message) {
		this.status = status;
		this.code = code;
		this.message = message;
	}

	public static ResponseEntity<ErrorResponse> toErrorResponseEntity(ErrorCode code, String message) {
		ErrorResponse response = ErrorResponse.builder()
			.status(code.getStatus())
			.code(code)
			.message(message)
			.build();

		return ResponseEntity.status(response.getStatus()).body(response);
	}
}
