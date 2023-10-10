package swmaestro.spaceodyssey.weddingmate.domain.users.controller;

import static org.springframework.http.HttpHeaders.*;
import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.AccessTokenDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.AuthService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Slf4j
@RestController
@RequestMapping("/api/v1/token")
@RequiredArgsConstructor
public class AuthController {

	private final AuthService authService;

	@PostMapping("/refresh")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> refresh(HttpServletRequest request) {

		AccessTokenDto resDto = authService.refresh(request);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resDto)
			.build();
	}

	@PostMapping("/logout")
	@ResponseStatus(HttpStatus.OK)
	public ResponseEntity<ApiResponse<Object>> logout(@RequestBody AccessTokenDto requestDto) {
		authService.logout(requestDto);
		ResponseCookie responseCookie = removeRefreshTokenCookie();

		return ResponseEntity.ok()
			.header(SET_COOKIE, responseCookie.toString())
			.body(ApiResponse.builder()
				.status(ApiResponseStatus.SUCCESS)
				.data(LOGOUT_SUCCESS)
				.build());
	}

	@DeleteMapping("/signout")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> signout(@AuthUsers Users users, @RequestBody AccessTokenDto requestDto) {
		authService.signout(users, requestDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(SIGNOUT_SUCCESS)
			.build();
	}

	public ResponseCookie removeRefreshTokenCookie() {

		return ResponseCookie.from("refreshToken", null)
			.path("/") // 해당 경로 하위의 페이지에서만 쿠키 접근 허용. 모든 경로에서 접근 허용한다.
			.maxAge(0) // 쿠키의 expiration 타임을 0으로 하여 없앤다.
			.secure(true) // HTTPS로 통신할 때만 쿠키가 전송된다.
			.httpOnly(true) // JS를 통한 쿠키 접근을 막아, XSS 공격 등을 방어하기 위한 옵션이다.
			.build();
	}
}