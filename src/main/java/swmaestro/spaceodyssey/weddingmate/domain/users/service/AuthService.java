package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.AccessTokenDto;
import swmaestro.spaceodyssey.weddingmate.global.config.jwt.JwtTokenProvider;
import swmaestro.spaceodyssey.weddingmate.global.config.redis.RedisService;
import swmaestro.spaceodyssey.weddingmate.global.exception.token.RefreshTokenCookieNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.token.RefreshTokenNotEqualException;
import swmaestro.spaceodyssey.weddingmate.global.exception.token.UnAuthorizedRefreshTokenException;
import swmaestro.spaceodyssey.weddingmate.global.utils.CookieUtils;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class AuthService {

	private final String COOKIE_KEY = "refreshToken";
	private final JwtTokenProvider jwtProvider;
	private final RedisService redisService;

	public AccessTokenDto refresh(HttpServletRequest request) {

		String oldRefreshToken = CookieUtils.getCookie(request, COOKIE_KEY)
			.map(Cookie::getValue).orElseThrow(RefreshTokenCookieNotFoundException::new);

		// 들어온 refreshToekn 검증
		if (!jwtProvider.validateToken(oldRefreshToken)) {
			throw new UnAuthorizedRefreshTokenException();
		}
		// refreshToken에서 Authentication 추출하기
		Authentication authentication = jwtProvider.getAuthentication(oldRefreshToken);

		// Redis의 RefreshToken을 가져오면서, 로그아웃된 사용자인 경우 예외 처리
		String savedRefreshToken = redisService.getRefreshToken(authentication.getName())
			.orElseThrow(() -> new RuntimeException(authentication.getName()));

		// 저장되어있던 refreshToken과 일치하는지 확인
		System.out.println(oldRefreshToken);
		System.out.println(savedRefreshToken);
		if (!oldRefreshToken.equals(savedRefreshToken)) {
			throw new RefreshTokenNotEqualException();
		}

		// 토큰 생성을 위해 accessToken에서 Claims 추출
		String newAccessToken = jwtProvider.createAccessToken(authentication.getName());

		return new AccessTokenDto(newAccessToken);
	}

	public AccessTokenDto signOut(AccessTokenDto requestDto) {
		// accessToken에서 Authentication 추출하기
		String accessToken = requestDto.getAccessToken();
		Authentication authentication = jwtProvider.getAuthentication(accessToken);

		// Redis의 RefreshToken을 가져오면서, 이미 로그아웃된 사용자인 경우 예외 처리
		String refreshToken = redisService.getRefreshToken(authentication.getName())
			.orElseThrow(() -> new RuntimeException("RefreshToken NotFound:" + authentication.getName()));

		// AccessToken의 남은 시간 추출 후 BlackList에 저장
		Long remainingTime = jwtProvider.getRemainingTime(accessToken);
		redisService.setData("BlackList:" + accessToken, "signOut", remainingTime);
		redisService.deleteData("RefreshToken:" + authentication.getName());

		return new AccessTokenDto(accessToken);
	}

}
