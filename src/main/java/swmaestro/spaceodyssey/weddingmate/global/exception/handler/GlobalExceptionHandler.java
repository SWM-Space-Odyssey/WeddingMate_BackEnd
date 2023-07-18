package swmaestro.spaceodyssey.weddingmate.global.exception.handler;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2AuthProviderIdNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2DuplicateEmailException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuthUnauthUrlException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNameNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;

@RestControllerAdvice
public class GlobalExceptionHandler {

	/*================== Basic Exception ==================*/
	@ExceptionHandler(RuntimeException.class)
	protected final ResponseEntity<ErrorResponse> handleRunTimeException(RuntimeException e) {
		final ErrorResponse response = ErrorResponse.builder()
			.status(HttpStatus.BAD_REQUEST)
			.code(ErrorCode.RUNTIME_EXCEPTION)
			.message(e.getMessage())
			.build();
		return ResponseEntity.status(response.getStatus()).body(response);
	}

	/*================== Oauth2 Exception ==================*/
	@ExceptionHandler(OAuth2AuthProviderIdNotFoundException.class)
	protected final ResponseEntity<ErrorResponse> handleOAuth2ProviderIdNotFoundException(
		OAuth2AuthProviderIdNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.OAUTH_PROVIDERID_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(OAuth2DuplicateEmailException.class)
	protected final ResponseEntity<ErrorResponse> handleOAuth2DuplicateEmailException(OAuth2DuplicateEmailException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.OAUTH_DUPLICATE_EMAIL, e.getMessage());
	}

	@ExceptionHandler(OAuthUnauthUrlException.class)
	protected final ResponseEntity<ErrorResponse> handleOAuthUnauthUrlException(OAuthUnauthUrlException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.OAUTH_UNAUTHORIZED_URL, e.getMessage());
	}

	/*================== User Exception ==================*/
	@ExceptionHandler(UserNotFoundException.class)
	protected final ResponseEntity<ErrorResponse> handleUserNotFoundException(UserNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.USER_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(UserNameNotFoundException.class)
	protected final ResponseEntity<ErrorResponse> handleUserNameNotFoundException(UserNameNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.USER_NAME_NOTFOUN, e.getMessage());
	}

}
