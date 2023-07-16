package swmaestro.spaceodyssey.weddingmate.domain.oauth2.handler;

import static swmaestro.spaceodyssey.weddingmate.domain.oauth2.repository.CookieAuthorizationRequestRepository.*;

import java.io.IOException;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.repository.CookieAuthorizationRequestRepository;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.utils.CookieUtils;

@Component
@RequiredArgsConstructor
public class OAuth2AuthenticationFailureHandler extends SimpleUrlAuthenticationFailureHandler {

	private final CookieAuthorizationRequestRepository cookieAuthorizationRequestRepository;

	@Override
	public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response,
		AuthenticationException exception) throws IOException, ServletException {
		String targetUrl = CookieUtils.getCookie(request, REDIRECT_URI_PARAM_COOKIE_NAME)
			.map(Cookie::getValue)
			.orElse("/");

		targetUrl = UriComponentsBuilder.fromUriString(targetUrl)
			.queryParam("error", exception.getLocalizedMessage())
			.build().toUriString();

		cookieAuthorizationRequestRepository.removeAuthorizationRequestCookies(request, response);

		getRedirectStrategy().sendRedirect(request, response, targetUrl);
	}

}
