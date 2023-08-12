package swmaestro.spaceodyssey.weddingmate.global.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public enum ApiResponseStatus {
	SUCCESS("success"),
	FAIL("fail"),
	ERROR("error");

	private String value;

	ApiResponseStatus(String value){
		this.value = value;
	}
}
