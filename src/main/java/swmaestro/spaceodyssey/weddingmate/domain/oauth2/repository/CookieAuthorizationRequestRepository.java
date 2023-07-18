package swmaestro.spaceodyssey.weddingmate.domain.oauth2.repository;

import org.springframework.security.oauth2.client.web.AuthorizationRequestRepository;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationRequest;
import org.springframework.stereotype.Component;

import com.nimbusds.oauth2.sdk.util.StringUtils;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.utils.CookieUtils;

@Component
public class CookieAuthorizationRequestRepository implements AuthorizationRequestRepository {

	public static final String OAUTH2_AUTHORIZATION_REQUEST_COOKIE_NAME = "oauth2_auth_request";
	public static final String REDIRECT_URI_PARAM_COOKIE_NAME = "redirect_uri";
	private static final int COOKIE_EXPIRE_SECONDS = 180;

	/* cookie에 저장되어 있던 authorizationRequest들을 가져온다
	=> authorizationUri, authorizationGrantType, responseType, cliendId, redirectUri, scope, additionalParameters */
	@Override
	public OAuth2AuthorizationRequest loadAuthorizationRequest(HttpServletRequest request) {
		return CookieUtils.getCookie(request, OAUTH2_AUTHORIZATION_REQUEST_COOKIE_NAME)
			.map(cookie -> CookieUtils.deserialize(cookie, OAuth2AuthorizationRequest.class))
			.orElse(null);
	}

	/* 플랫폼으로 보내기 위한 request를 'oauth_auth_request'라는 cookie에 저장한다
	=> authorizationUri, authorizationGrantType, responseType, cliendId, redirectUri, scope, additionalParameters */
	@Override
	public void saveAuthorizationRequest(OAuth2AuthorizationRequest authorizationRequest, HttpServletRequest request,
		HttpServletResponse response) {
		if (authorizationRequest == null) {
			removeAuthorizationRequest(request, response);
			return;
		}

		CookieUtils.addCookie(response, OAUTH2_AUTHORIZATION_REQUEST_COOKIE_NAME,
			CookieUtils.serialize(authorizationRequest), COOKIE_EXPIRE_SECONDS);

		/*
		 * http://localhost:8080/oauth2/authorize/kakao?redirect_uri=http://localhost:3000/oauth/redirect 로 요청 받았을 때
		 * http://localhost:3000/oauth/redirect 를 가져온다
		 * 그리고 존재하는 경우 cookie 에 넣어준다
		 */
		String redirectUriAfterLogin = request.getParameter(REDIRECT_URI_PARAM_COOKIE_NAME);
		if (StringUtils.isNotBlank(redirectUriAfterLogin)) {
			CookieUtils.addCookie(response, REDIRECT_URI_PARAM_COOKIE_NAME,
				redirectUriAfterLogin, COOKIE_EXPIRE_SECONDS);
		}
	}

	/** remove를 재정의 해서
	 * 1) cookie를 가져오고
	 * 2) remove는 successHandler 또는 failureHandler에서 할 수 있도록 한다 */
	@Override
	public OAuth2AuthorizationRequest removeAuthorizationRequest(HttpServletRequest request,
		HttpServletResponse response) {
		return this.loadAuthorizationRequest(request);
	}

	/**
	 * 사용자 정보를 다 가지고 온 뒤 redirect를 하면 기존에 남아있던 쿠키들을 제거해주기 위해 사용된다
	 *  OAuth2AuthorizationRequest와 클라이언트에서 파리미터로 요청한 redirect_uri가 된다
	 * */
	public void removeAuthorizationRequestCookies(HttpServletRequest request, HttpServletResponse response) {
		CookieUtils.deleteCookie(request, response, OAUTH2_AUTHORIZATION_REQUEST_COOKIE_NAME);
		CookieUtils.deleteCookie(request, response, REDIRECT_URI_PARAM_COOKIE_NAME);
	}
}
