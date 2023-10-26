package swmaestro.spaceodyssey.weddingmate.global.constant;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ResponseConstant {

	// USER
	public static final String LOGIN_SUCCESS = "성공적으로 로그인되었습니다.";

	/* USERS */
	// planner
	public static final String PLANNER_SIGNUP_SUCCESS = " 플래너님의 회원가입이 완료되었습니다.";

	// customer
	public static final String CUSTOMER_SIGNUP_SUCCESS = " 고객님의 회원가입이 완료되었습니다.";

	public static final String LOGOUT_SUCCESS = "로그아웃에 성공했습니다.";
	public static final String SIGNOUT_SUCCESS = "회원 탈퇴에 성공했습니다.";

	// accountStatus

	public static final String ACCOUNT_SUSPENDED_MESSAGE = "정지된 계정입니다.";
	public static final String ACCOUNT_WITHDRAW_MESSAGE = "이미 탈퇴한 계정입니다.";
	public static final String ACCOUNT_BANNED_MESSAGE = "재가입 불가능한 계정입니다.";

	// exception
	public static final String USER_NOTFOUND = "해당 유저를 찾을 수 없습니다.";
	public static final String USER_NAME_NOTFOUND = "해당 유저의 이름을 찾을 수 없습니다(CustomUserDetailsService)";
	public static final String USER_UNAUTHORIZED = "권한이 없는 유저입니다";
	public static final String PLANNER_NOTFOUND = "해당 플래너를 찾을 수 없습니다.";
	public static final String PLANNER_PROFILE_NOTFOUND = "해당 플래너의 프로필을 찾을 수 없습니다.";
	public static final String CUSTOMER_NOTFOUND = "해당 고객을 찾을 수 없습니다";

	public static final String PLANNER_DUPLICATE_REGISTRATION = "이미 플래너로 가입하셨습니다.";
	public static final String CUSTOMER_DUPLICATE_REGISTRATION = "이미 예비부부로 가입하셨습니다.";

	/* OAUTH */
	public static final String OAUTH_PROVIDERID_NOTFOUND = "OAuth2 Provider에서 ProviderId을 찾을 수 없습니다.";
	public static final String OAUTH_DUPLICATE_EMAIL = "회원가입을 통해 이미 가입하였습니다. 해당 SNS 계정으로 다시 시도해주세요.";
	public static final String OAUTH_UNAUTHORIZED_URL = "해당 url은 인증되지 않았습니다.";

	/* PROFILE */
	public static final String PROFILE_MODIFICATION_NOT_ALLOWED = "해당 프로필의 작성자가 아닙니다.";
	public static final String CUSTOMER_PROFILE_UPDATE_SUCCESS = "해당 고객 프로필을 수정했습니다.";

	/* PORTFOLIO */
	public static final String PORTFOLIO_NOTFOUND = "해당 포트폴리오를 찾을 수 없습니다.";
	public static final String PORTFOLIO_CREATE_SUCCESS = "해당 포트폴리오를 생성했습니다.";
	public static final String PORTFOLIO_UPDATE_SUCCESS = "해당 포트폴리오를 수정했습니다.";
	public static final String PORTFOLIO_DELETE_SUCCESS = "해당 포트폴리오를 삭제했습니다.";

	/* ITEM */
	public static final String ITEM_NOTFOUND = "해당 아이템을 찾을 수 없습니다.";
	public static final String ITEM_CREATE_SUCCESS = "해당 아이템을 생성했습니다.";
	public static final String ITEM_UPDATE_SUCCESS = "해당 아이템을 수정했습니다.";
	public static final String ITEM_DELETE_SUCCESS = "해당 아이템을 삭제했습니다.";

	/* CATEGORY */
	public static final String CATEGORY_NOTFOUND = "카테고리를 찾을 수 없습니다.";

	/* Token */
	public static final String REFRESH_TOKEN_UNAUTHORIZED = "Refresh Token 검증에 실패했습니다.";
	public static final String REFRESH_TOKEN_NOT_EQUAL = "저장된 Refresh Token과 일치하지 않습니다.";
	public static final String REFRESH_TOKEN_COOKIE_NOTFOUND = "Refresh Token이 담긴 쿠키를 찾을 수 없습니다.";

	/* File */
	public static final String FILE_NOTFOUND = "파일을 찾을 수 없습니다";
	public static final String EMPTY_FILE_NAME = "파일명이 비어있습니다.";
	public static final String FILE_NOT_IMAGE = "이미지 파일이 아닙니다. 이미지 파일만 올릴 수 있습니다.";
	public static final String UNSUPPORTED_FILE_EXTENSION = "지원하지 않는 파일 확장자입니다.";
	public static final String FILE_UPLOAD_FAILURE = "파일 업로드 과정에서 오류가 발생했습니다.";
	public static final String FILE_IMAGE_READ_FAILURE = " 이미지 파일을 읽는 과정에서 오류가 발생했습니다.";
	public static final String FILE_KAKAO_PROFILE_DOWNLOAD_FAILURE = "카카오 프로필 이미지를 다운받는데 실패했습니다.";
	public static final String FILE_MALFORMED_URL = "잘못된 파일 URL입니다.";

	/* Like */
	public static final String LIKE_SUCCESS = "좋아요가 되었습니다.";
	public static final String UNLIKE_SUCCESS = "좋아요가 취소되었습니다.";
	public static final String LIKE_TYPE_NOT_SUPPORTED = "지원되지 않는 좋아요 타입입니다.";

	/* CHAT */
	public static final String CHATROOM_NOTFOUND = "채팅방을 찾을 수 없습니다.";
	public static final String CHATROOM_DELETE_SUCCESS = "해당 채팅방을 삭제했습니다.";
	public static final String CHATROOM_NOT_AUTHORIZED = "해당 채팅방의 주인이 아닙니다.";

	/* COMPANY */
	public static final String COMPANY_NOTFOUND = "업체를 찾을 수 없습니다.";

	/* REPORT */
	public static final String REPORT_CREATE_SUCCESS = "신고가 성공적으로 접수되었습니다.";
}
