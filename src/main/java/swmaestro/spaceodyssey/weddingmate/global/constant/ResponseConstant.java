package swmaestro.spaceodyssey.weddingmate.global.constant;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ResponseConstant {

	// USER
	public static final String LOGIN_SUCCESS = "성공적으로 로그인되었습니다.";

	/* USERS */

	// exception
	public static final String USER_NOTFOUND = "해당 유저를 찾을 수 없습니다.";
	public static final String USER_NAME_NOTFOUND = "해당 유저의 이름을 찾을 수 없습니다(CustomUserDetailsService)";
	public static final String USER_UNAUTHORIZED = "권한이 없는 유저입니다";
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
}
