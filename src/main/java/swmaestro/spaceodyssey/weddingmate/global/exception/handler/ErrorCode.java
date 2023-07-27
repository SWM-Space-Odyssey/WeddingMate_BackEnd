package swmaestro.spaceodyssey.weddingmate.global.exception.handler;

import static org.springframework.http.HttpStatus.*;

import org.springframework.http.HttpStatus;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {

	/* Common */
	// Basic - C0***
	RUNTIME_EXCEPTION(BAD_REQUEST, "C0001", "RUNTIME_EXCEPTION"),

	// Oauth - C1***
	OAUTH_PROVIDERID_NOTFOUND(NOT_FOUND, "C1001", "OAUTH_PROVIDERID_NOTFOUND"),
	OAUTH_DUPLICATE_EMAIL(CONFLICT, "C1002", "OAUTH_DUPLICATE_EMAIL"),
	OAUTH_UNAUTHORIZED_URL(UNAUTHORIZED, "C1003", "OAUTH_UNAUTHORIZED_URL"),

	// User - C2***
	USER_NOTFOUND(NOT_FOUND, "C2001", "USER_NOTFOUND"),
	USER_NAME_NOTFOUND(NOT_FOUND, "C2002", "USER_NAME_NOTFOUND"),
	USER_UNAUTHORIZED(UNAUTHORIZED, "C2003", "USER_UNAUTHORIZED"),

	// Token - C3***
	REFRESH_TOKEN_UNAUTHORIZED(UNAUTHORIZED, "C3001", "REFRESH_TOKEN_UNAUTHORIZED"),

	REFRESH_TOKEN_NOTFOUND(NOT_FOUND, "C3002", "REFRESH_TOKEN_NOTFOUND"),

	// Portfolio - C4***

	PORTFOLIO_NOTFOUND(NOT_FOUND, "C4001", "PORTFOLIO_NOTFOUND"),
	ITEM_NOTFOUND(NOT_FOUND, "C4002", "ITEM_NOTFOUND"),

	// Category - C5***
	CATEGORY_NOTFOUND(NOT_FOUND, "C5001", "CATEGORY_NOTFOUND"),

	// File - C6***
	FILE_NOTFOUND(NOT_FOUND, "C6001", "FILE_NOTFOUND");

	/* EXCEPTION */


	private final HttpStatus status;
	private final String code;
	private final String message;
}
