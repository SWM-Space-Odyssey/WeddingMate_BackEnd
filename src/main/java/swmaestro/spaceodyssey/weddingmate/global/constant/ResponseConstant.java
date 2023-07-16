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

	/* OAUTH */
	public static final String OAUTH_NOTFOUND_PROVIDERID = "OAuth2 Provider에서 ProviderId을 찾을 수 없습니다.";
	public static final String OAUTH_DUPLICATE_EMAIL = "회원가입을 통해 이미 가입하였습니다. 해당 SNS 계정으로 다시 시도해주세요.";
	public static final String OAUTH_UNAUTHORIZED_URL = "해당 url은 인증되지 않았습니다.";
}
