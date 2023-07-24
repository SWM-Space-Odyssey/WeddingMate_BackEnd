package swmaestro.spaceodyssey.weddingmate.global.exception.handler;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import swmaestro.spaceodyssey.weddingmate.global.exception.category.CategoryNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2AuthProviderIdNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2DuplicateEmailException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuthUnauthUrlException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.token.RefreshTokenNotEqualException;
import swmaestro.spaceodyssey.weddingmate.global.exception.token.UnAuthorizedRefreshTokenException;
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
		return ErrorResponse.toErrorResponseEntity(ErrorCode.USER_NAME_NOTFOUND, e.getMessage());
	}

	/*================== Token Exception ==================*/
	@ExceptionHandler(RefreshTokenNotEqualException.class)
	protected final ResponseEntity<ErrorResponse> handleRefreshTokenNotEqualException(RefreshTokenNotEqualException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.REFRESH_TOKEN_UNAUTHORIZED, e.getMessage());
	}

	@ExceptionHandler(UnAuthorizedRefreshTokenException.class)
	protected final ResponseEntity<ErrorResponse> handlerUnAuthorizedRefreshTokenException(UnAuthorizedRefreshTokenException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.REFRESH_TOKEN_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(PortfolioNotFoundException.class)
	protected final ResponseEntity<ErrorResponse> handlePortfolioNotFoundException(PortfolioNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.PORTFOLIO_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(ItemNotFoundException.class)
	protected final ResponseEntity<ErrorResponse> handleItemNotFoundException(ItemNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.ITEM_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(CategoryNotFoundException.class)
	protected final ResponseEntity<ErrorResponse> handleCategoryNotFoundException(CategoryNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.CATEGORY_NOTFOUND, e.getMessage());
	}

}
