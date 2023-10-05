package swmaestro.spaceodyssey.weddingmate.global.config.jwt;

import java.io.IOException;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.global.config.redis.RedisService;

@Component
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {

	private final JwtTokenProvider jwtTokenProvider;
	private final RedisService redisService;

	private static final Logger log = LoggerFactory.getLogger(JwtAuthenticationFilter.class);

	@Override
	public void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
		FilterChain filterChain) throws
		IOException,
		ServletException {

		try {
			// 1. Request Header에서 Jwt Token 추출
			String token = getJwtFromRequest(request);

			// 2. Validate method로 token 유효성 검사
			if (StringUtils.hasText(token) && jwtTokenProvider.validateToken(token)) {
				// BlackList에 존재하는 토큰으로 요청이 온 경우
				Optional<String> isBlackList = redisService.getBlackList(token);
				if (isBlackList.isPresent()){
					throw new RuntimeException("이미 만료된 토큰입니다.");
				}

				Authentication authentication = jwtTokenProvider.getAuthentication(token);
				SecurityContextHolder.getContext().setAuthentication(authentication);
			}
		} catch (Exception ex) {
			log.error("Security Context에서 user authentication 설정 불가", ex);
		}
		filterChain.doFilter(request, response);
	}

	public String getJwtFromRequest(HttpServletRequest request) {
		String bearerToken = request.getHeader("Authorization");
		if (StringUtils.hasText(bearerToken) && bearerToken.startsWith("Bearer")) {
			return bearerToken.substring(7);
		}
		return null;
	}
}
