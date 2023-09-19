package swmaestro.spaceodyssey.weddingmate.domain.oauth2.handler;

import static swmaestro.spaceodyssey.weddingmate.domain.oauth2.service.CookieAuthorizationRequestRepository.*;

import java.io.IOException;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.springframework.http.ResponseCookie;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.UserPrincipal;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.service.CookieAuthorizationRequestRepository;
import swmaestro.spaceodyssey.weddingmate.global.config.jwt.JwtTokenProvider;
import swmaestro.spaceodyssey.weddingmate.global.config.redis.RedisService;
import swmaestro.spaceodyssey.weddingmate.global.utils.CookieUtils;

@Slf4j
@Component
@RequiredArgsConstructor
public class OAuth2AuthenticationSuccessHandler extends SimpleUrlAuthenticationSuccessHandler {

	private final JwtTokenProvider tokenProvider;
	private final RedisService redisService;
	private final CookieAuthorizationRequestRepository cookieAuthorizationRequestRepository;

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
		Authentication authentication) throws IOException, ServletException {

		UserPrincipal userPrincipal = (UserPrincipal)authentication.getPrincipal();
		String email = userPrincipal.getEmail();

		String targetUrl = determineTargetUrl(request, response, authentication);
		String redirectUrl = makeRedirectUrl(email, targetUrl);

		ResponseCookie responseCookie = generateRefreshTokenCookie(email);
		response.setHeader("Set-Cookie", responseCookie.toString());
		response.getWriter().write(redirectUrl);

		log.info("oauth2 redirectUrl " + redirectUrl);
		if (response.isCommitted()) {
			logger.debug("응답이 이미 커밋된 상태입니다. " + redirectUrl + "로 리다이렉트하도록 바꿀 수 없습니다.");
			return;
		}

		clearAuthenticationAttributes(request, response);
		getRedirectStrategy().sendRedirect(request, response, redirectUrl);
	}

	protected String determineTargetUrl(HttpServletRequest request, HttpServletResponse response,
		Authentication authentication) {
		Optional<String> redirectUrl = CookieUtils.getCookie(request, REDIRECT_URL_PARAM_COOKIE_KEY)
			.map(Cookie::getValue);

		String targetUrl = redirectUrl.orElse(getDefaultTargetUrl());
		return UriComponentsBuilder.fromUriString(targetUrl)
			.build().toUriString();
	}

	private String makeRedirectUrl(String email, String redirectUrl) {
		if (redirectUrl.equals(getDefaultTargetUrl())) {
			redirectUrl = "https://dev.weddingmate.co.kr";
		}

		String accessToken = tokenProvider.createAccessToken(email);

		return UriComponentsBuilder.fromHttpUrl(redirectUrl)
			.path("/oauth2/redirect")
			.queryParam("accessToken", accessToken)
			.queryParam("redirectUrl", redirectUrl)
			.build()
			.encode() // percent-endcoding
			.toUriString();
	}

	protected void clearAuthenticationAttributes(HttpServletRequest request, HttpServletResponse response) {
		super.clearAuthenticationAttributes(request);

		cookieAuthorizationRequestRepository.removeAuthorizationRequest(request, response);
	}

	public ResponseCookie generateRefreshTokenCookie(String email) {

		String refreshToken = tokenProvider.createRefreshToken(email);
		Long refreshTokenValidationTime = tokenProvider.getRefreshTokenValidationMs();

		redisService.setData("RefreshToken:" + email, refreshToken, refreshTokenValidationTime);

		return ResponseCookie.from("refreshToken", refreshToken)
			.path("/") // 해당 경로 하위의 페이지에서만 쿠키 접근 허용. 모든 경로에서 접근 허용한다.
			.domain(".weddingmate.co.kr")
			.maxAge(TimeUnit.MILLISECONDS.toSeconds(refreshTokenValidationTime)) // 쿠키 만료 시기(초). 없으면 브라우저 닫힐 때 제거
			.secure(true) // HTTPS로 통신할 때만 쿠키가 전송된다.
			.sameSite("None") // 크로스 사이트에도 쿠키 전송 가능
			.httpOnly(true) // JS를 통한 쿠키 접근을 막아, XSS 공격 등을 방어하기 위한 옵션이다.
			.build();
	}
}
