package swmaestro.spaceodyssey.weddingmate.global.exception.handler;

import org.springframework.http.HttpStatus;

import lombok.Builder;
import lombok.Getter;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Getter
public class ErrorResponse {
	private HttpStatus status;
	private ErrorCode code;
	private String message;

	@Builder
	public ErrorResponse(HttpStatus status, ErrorCode code, String message) {
		this.status = status;
		this.code = code;
		this.message = message;
	}

	public static ApiResponse<Object> toErrorResponseEntity(ErrorCode code, String message) {
		ErrorResponse response = ErrorResponse.builder()
			.status(code.getStatus())
			.code(code)
			.message(message)
			.build();

		return ApiResponse.builder()
			.status(ApiResponseStatus.FAIL)
			.data(response)
			.build();
	}
}
