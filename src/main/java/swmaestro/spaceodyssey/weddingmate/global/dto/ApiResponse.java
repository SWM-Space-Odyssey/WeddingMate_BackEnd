package swmaestro.spaceodyssey.weddingmate.global.dto;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@JsonInclude(JsonInclude.Include.NON_NULL) // null값이면 응답에서 제외
public class ApiResponse<T> {
	private ApiResponseStatus status; // success, fail, error
	private String message;
	private T data; // errorResponse

	@Builder
	public ApiResponse(ApiResponseStatus status, String message, T data) {
		this.status = status;
		this.message = message;
		this.data = data;
	}

	@Builder
	public ApiResponse(ApiResponseStatus status, T data) {
		this.status = status;
		this.data = data;
	}
}
