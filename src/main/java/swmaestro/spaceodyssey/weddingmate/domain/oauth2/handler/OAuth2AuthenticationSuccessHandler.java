package swmaestro.spaceodyssey.weddingmate.domain.oauth2.handler;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import org.springframework.http.ResponseCookie;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.UserPrincipal;
import swmaestro.spaceodyssey.weddingmate.global.config.jwt.JwtTokenProvider;
import swmaestro.spaceodyssey.weddingmate.global.config.redis.RedisService;

@Component
@RequiredArgsConstructor
public class OAuth2AuthenticationSuccessHandler extends SimpleUrlAuthenticationSuccessHandler {

	private final JwtTokenProvider tokenProvider;
	private final RedisService redisService;

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
		Authentication authentication) throws IOException, ServletException {

		UserPrincipal userPrincipal = (UserPrincipal)authentication.getPrincipal();
		String email = userPrincipal.getEmail();

		String targetUrl = makeRedirectUrl(email);
		ResponseCookie responseCookie = generateRefreshTokenCookie(email);

		response.setHeader("Set-Cookie", responseCookie.toString());
		response.getWriter().write(targetUrl);

		if (response.isCommitted()) {
			logger.debug("Response has already been committed. Unable to redirect to " + targetUrl);
			return;
		}

		getRedirectStrategy().sendRedirect(request, response, targetUrl);
	}

	private String makeRedirectUrl(String email) {
		String accessToken = tokenProvider.createAccessToken(email);

		return UriComponentsBuilder.fromHttpUrl("http://weddingmate.co.kr")
			.queryParam("accessToken", accessToken)
			.build()
			.encode()
			.toUriString();
	}

	public ResponseCookie generateRefreshTokenCookie(String email) {

		String refreshToken = tokenProvider.createRefreshToken(email);
		Long refreshTokenValidationTime = tokenProvider.getRefreshTokenValidationMs();

		redisService.setData("RefreshToken: "+ email, refreshToken, refreshTokenValidationTime);

		return ResponseCookie.from("refreshToken", refreshToken)
			.path("/") // 해당 경로 하위의 페이지에서만 쿠키 접근 허용. 모든 경로에서 접근 허용한다.
			.domain(".com")
			.maxAge(TimeUnit.MILLISECONDS.toSeconds(refreshTokenValidationTime)) // 쿠키 만료 시기(초). 없으면 브라우저 닫힐 때 제거
			.secure(true) // HTTPS로 통신할 때만 쿠키가 전송된다.
			.sameSite("none")
			.httpOnly(true) // JS를 통한 쿠키 접근을 막아, XSS 공격 등을 방어하기 위한 옵션이다.
			.build();
	}
}
