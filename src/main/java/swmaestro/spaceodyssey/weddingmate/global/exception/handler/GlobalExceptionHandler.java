package swmaestro.spaceodyssey.weddingmate.global.exception.handler;

import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.exception.category.CategoryNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileKakaoProfileDownloadFailureException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileMalformedUrlException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNameEmptyException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileUnSupportedExtensionTypeException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotImageException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileUploadFailureException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2AuthProviderIdNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2DuplicateEmailException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuthUnauthUrlException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.ItemNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.PlannerProfileNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.ProfileModificationNotAllowedException;
import swmaestro.spaceodyssey.weddingmate.global.exception.token.RefreshTokenNotEqualException;
import swmaestro.spaceodyssey.weddingmate.global.exception.token.UnAuthorizedRefreshTokenException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.CustomerDuplicateRegistrationException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerDuplicateRegistrationException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNameNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

@RestControllerAdvice
public class GlobalExceptionHandler {

	/*================== Basic Exception ==================*/
	@ExceptionHandler(RuntimeException.class)
	protected final ApiResponse<Object> handleRunTimeException(RuntimeException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.RUNTIME_EXCEPTION, e.getMessage());
	}

	/*================== Oauth2 Exception ==================*/
	@ExceptionHandler(OAuth2AuthProviderIdNotFoundException.class)
	protected final ApiResponse<Object> handleOAuth2ProviderIdNotFoundException(
		OAuth2AuthProviderIdNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.OAUTH_PROVIDERID_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(OAuth2DuplicateEmailException.class)
	protected final ApiResponse<Object> handleOAuth2DuplicateEmailException(OAuth2DuplicateEmailException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.OAUTH_DUPLICATE_EMAIL, e.getMessage());
	}

	@ExceptionHandler(OAuthUnauthUrlException.class)
	protected final ApiResponse<Object> handleOAuthUnauthUrlException(OAuthUnauthUrlException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.OAUTH_UNAUTHORIZED_URL, e.getMessage());
	}

	/*================== User Exception ==================*/
	@ExceptionHandler(UserNotFoundException.class)
	protected final ApiResponse<Object> handleUserNotFoundException(UserNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.USER_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(UserNameNotFoundException.class)
	protected final ApiResponse<Object> handleUserNameNotFoundException(UserNameNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.USER_NAME_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(UserUnAuthorizedException.class)
	protected final ApiResponse<Object> handleUserUnAuthorizedException(UserUnAuthorizedException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.USER_UNAUTHORIZED, e.getMessage());
	}

	// Planner Exception
	@ExceptionHandler(PlannerNotFoundException.class)
	protected final ApiResponse<Object> handlerPlannerNotFoundException(PlannerNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.PLANNER_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(PlannerDuplicateRegistrationException.class)
	protected final ApiResponse<Object> handlePlannerDuplicateRegistrationException(
		PlannerDuplicateRegistrationException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.PLANNER_DUPLICATE_REGISTRATION, e.getMessage());
	}

	// Customer Exception
	@ExceptionHandler(CustomerDuplicateRegistrationException.class)
	protected final ApiResponse<Object> handleCustomerDuplicateResgistrationException(
		CustomerDuplicateRegistrationException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.CUSTOMER_DUPLICATE_REGISTRATION, e.getMessage());
	}

	/*================== Profile Exception ==================*/
	@ExceptionHandler(PlannerProfileNotFoundException.class)
	protected final ApiResponse<Object> handlePlannerProfileNotFoundException(
		PlannerProfileNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.PLANNER_PROFILE_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(ProfileModificationNotAllowedException.class)
	protected final ApiResponse<Object> handleProfileModificationNotAllowedException(
		ProfileModificationNotAllowedException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.PROFILE_MODIFICATION_NOT_ALLOWED, e.getMessage());
	}

	/*================== Token Exception ==================*/
	@ExceptionHandler(RefreshTokenNotEqualException.class)
	protected final ApiResponse<Object> handleRefreshTokenNotEqualException(RefreshTokenNotEqualException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.REFRESH_TOKEN_UNAUTHORIZED, e.getMessage());
	}

	@ExceptionHandler(UnAuthorizedRefreshTokenException.class)
	protected final ApiResponse<Object> handlerUnAuthorizedRefreshTokenException(
		UnAuthorizedRefreshTokenException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.REFRESH_TOKEN_NOTFOUND, e.getMessage());
	}

	/*================== Portfolio Exception ==================*/
	@ExceptionHandler(PortfolioNotFoundException.class)
	protected final ApiResponse<Object> handlePortfolioNotFoundException(PortfolioNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.PORTFOLIO_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(ItemNotFoundException.class)
	protected final ApiResponse<Object> handleItemNotFoundException(ItemNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.ITEM_NOTFOUND, e.getMessage());
	}

	/*================== Category Exception ==================*/
	@ExceptionHandler(CategoryNotFoundException.class)
	protected final ApiResponse<Object> handleCategoryNotFoundException(CategoryNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.CATEGORY_NOTFOUND, e.getMessage());
	}

	/*================== File Exception ==================*/
	@ExceptionHandler(FileNotFoundException.class)
	protected final ApiResponse<Object> handleFileNotFoundException(FileNotFoundException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.FILE_NOTFOUND, e.getMessage());
	}

	@ExceptionHandler(FileNameEmptyException.class)
	protected final ApiResponse<Object> handleFileNameEmptyException(FileNameEmptyException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.FILE_NAME_EMPTY, e.getMessage());
	}

	@ExceptionHandler(FileUnSupportedExtensionTypeException.class)
	protected final ApiResponse<Object> handleUnSupportedExtensionTypeException(
		FileUnSupportedExtensionTypeException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.FILE_UNSUPPORTED_EXTENSION, e.getMessage());
	}

	@ExceptionHandler(FileNotImageException.class)
	protected final ApiResponse<Object> handleFileNotImageException(FileNotImageException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.FILE_NOT_IMAGE, e.getMessage());
	}

	@ExceptionHandler(FileUploadFailureException.class)
	protected final ApiResponse<Object> handleFileUploadFailureException(FileUploadFailureException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.FILE_UPLOAD_FAILURE, e.getMessage());
	}

	@ExceptionHandler(FileMalformedUrlException.class)
	protected final ApiResponse<Object> handleFileMalformedUrlException(FileMalformedUrlException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.FILE_MALFORMED_URL, e.getMessage());
	}

	@ExceptionHandler(FileKakaoProfileDownloadFailureException.class)
	protected final ApiResponse<Object> handleKakaoProfileDownloadFailureException(
		FileKakaoProfileDownloadFailureException e) {
		return ErrorResponse.toErrorResponseEntity(ErrorCode.FILE_KAKAO_PROFILE_DOWNLOAD_FAILURE, e.getMessage());
	}
}