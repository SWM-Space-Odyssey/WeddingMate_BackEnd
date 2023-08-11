package swmaestro.spaceodyssey.weddingmate.domain.oauth2.service;

import org.springframework.security.oauth2.client.web.AuthorizationRequestRepository;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationRequest;
import org.springframework.stereotype.Component;

import com.nimbusds.oauth2.sdk.util.StringUtils;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.global.utils.CookieUtils;

@Slf4j
@Component
public class CookieAuthorizationRequestRepository
	implements AuthorizationRequestRepository<OAuth2AuthorizationRequest> {

	public static final String OAUTH2_AUTHORIZATION_REQUEST_COOKIE_KEY = "oauth2AuthRequest";
	public static final String REDIRECT_URL_PARAM_COOKIE_KEY = "redirect_uri";
	private static final int COOKIE_EXPIRE_SECONDS = 180;

	// 쿠키에 저장된 인증 요청 정보 가져오기
	@Override
	public OAuth2AuthorizationRequest loadAuthorizationRequest(HttpServletRequest request) {
		return CookieUtils.getCookie(request, OAUTH2_AUTHORIZATION_REQUEST_COOKIE_KEY)
			.map(cookie -> CookieUtils.deserialize(cookie, OAuth2AuthorizationRequest.class))
			.orElse(null);
	}

	// 주어진 OAuth2 인증 요청을 쿠키에 저장
	// 만약 인증 요청이 null이라면, 현재 쿠키에 저장된 인증 요청을 삭제
	@Override
	public void saveAuthorizationRequest(OAuth2AuthorizationRequest authorizationRequest, HttpServletRequest request,
		HttpServletResponse response) {
		if (authorizationRequest == null) {
			removeAuthorizationRequest(request, response);
			return;
		}
		CookieUtils.addCookie(response, OAUTH2_AUTHORIZATION_REQUEST_COOKIE_KEY,
			CookieUtils.serialize(authorizationRequest), COOKIE_EXPIRE_SECONDS);
		String redirectUriAfterLogin = request.getParameter(REDIRECT_URL_PARAM_COOKIE_KEY);
		if (StringUtils.isNotBlank(redirectUriAfterLogin)) {
			CookieUtils.addCookie(response, REDIRECT_URL_PARAM_COOKIE_KEY, redirectUriAfterLogin,
				COOKIE_EXPIRE_SECONDS);
		}
	}

	// 현재 HTTP Request에서 OAuth2 인증 요청을 삭제하고, 삭제된 요청을 반환
	@Override
	public OAuth2AuthorizationRequest removeAuthorizationRequest(HttpServletRequest request,
		HttpServletResponse response) {
		return this.loadAuthorizationRequest(request);
	}

	// 현재 HTTP 요청과 응답에서 OAuth2 인증 요청과 관련된 쿠키를 모두 삭제
	public void removeAuthorizationRequestCookies(HttpServletRequest request, HttpServletResponse response) {
		CookieUtils.deleteCookie(request, response, OAUTH2_AUTHORIZATION_REQUEST_COOKIE_KEY);
		CookieUtils.deleteCookie(request, response, REDIRECT_URL_PARAM_COOKIE_KEY);
	}
}
