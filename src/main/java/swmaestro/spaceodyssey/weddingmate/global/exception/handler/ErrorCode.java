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
	USER_NAME_NOTFOUN(NOT_FOUND, "C2002", "USER_NAME_NOTFOUND");

	/* EXCEPTION */

	private final HttpStatus status;
	private final String code;
	private final String message;
}

