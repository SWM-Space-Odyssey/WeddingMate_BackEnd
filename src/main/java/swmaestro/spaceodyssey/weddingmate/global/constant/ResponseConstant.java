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

	// exception
	public static final String USER_NOTFOUND = "해당 유저를 찾을 수 없습니다.";
	public static final String USER_NAME_NOTFOUND = "해당 유저의 이름을 찾을 수 없습니다(CustomUserDetailsService)";
	public static final String USER_UNAUTHORIZED = "권한이 없는 유저입니다";
	public static final String PLANNER_NOTFOUND = "해당 플래너를 찾을 수 없습니다.";
	public static final String PLANNER_PROFILE_NOTFOUND = "해당 플래너의 프로필을 찾을 수 없습니다.";

	public static final String PLANNER_DUPLICATE_REGISTRATION = "이미 플래너로 가입하셨습니다.";
	public static final String CUSTOMER_DUPLICATE_REGISTRATION = "이미 예비부부로 가입하셨습니다.";

	/* OAUTH */
	public static final String OAUTH_PROVIDERID_NOTFOUND = "OAuth2 Provider에서 ProviderId을 찾을 수 없습니다.";
	public static final String OAUTH_DUPLICATE_EMAIL = "회원가입을 통해 이미 가입하였습니다. 해당 SNS 계정으로 다시 시도해주세요.";
	public static final String OAUTH_UNAUTHORIZED_URL = "해당 url은 인증되지 않았습니다.";

	/* PORTFOLIO */
	public static final String PORTFOLIO_NOTFOUND = "포트폴리오를 찾을 수 없습니다";
	public static final String ITEM_NOTFOUND = "해당 아이템을 찾을 수 없습니다";

	/* CATEGORY */
	public static final String CATEGORY_NOTFOUND = "카테고리를 찾을 수 없습니다";

	/* Token */
	public static final String REFRESH_TOKEN_UNAUTHORIZED = "Refresh Token 검증에 실패했습니다.";
	public static final String REFRESH_TOKEN_NOT_EQUAL = "저장된 Refresh Token과 일치하지 않습니다.";

	/* File */
	public static final String FILE_NOTFOUND = "파일을 찾을 수 없습니다";
}
