package swmaestro.spaceodyssey.weddingmate.global.config.jwt;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Date;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Header;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.UnsupportedJwtException;
import io.jsonwebtoken.security.Keys;
import io.jsonwebtoken.security.SignatureException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.service.CustomUserDetailsService;

@Component
@Slf4j
@RequiredArgsConstructor
public class JwtTokenProvider {

	private final CustomUserDetailsService customUserDetailsService;

	@Value("${spring.jwt.secret}")
	private String secretKey;
	private final Long accessTokenValidationMs = 30 * 60 * 1000L;
	private final Long refreshTokenValidationMs = 15 * 24 * 60 * 60 * 1000L;

	public Long getRefreshTokenValidationMs() {
		return refreshTokenValidationMs;
	}

	public String createAccessToken(String email) {
		Claims claims = Jwts.claims()
			.setSubject(email)
			.setIssuedAt(new Date())
			.setExpiration(new Date(System.currentTimeMillis() + accessTokenValidationMs));

		return Jwts.builder()
			.setHeaderParam(Header.TYPE, Header.JWT_TYPE)
			.setClaims(claims)
			.signWith(getSignKey(secretKey), SignatureAlgorithm.HS256)
			.compact();
	}

	public String createRefreshToken(String email) {
		Claims claims = Jwts.claims()
			.setSubject(email)
			.setIssuedAt(new Date())
			.setExpiration(new Date(System.currentTimeMillis() + refreshTokenValidationMs));

		return Jwts.builder()
			.setHeaderParam(Header.TYPE, Header.JWT_TYPE)
			.setClaims(claims)
			.signWith(getSignKey(secretKey), SignatureAlgorithm.HS256)
			.compact();
	}

	private Key getSignKey(String secretKey) {
		return Keys.hmacShaKeyFor(secretKey.getBytes(StandardCharsets.UTF_8));
	}
	public boolean validateToken(String authToken) {
		try {
			Jwts.parserBuilder()
				.setSigningKey(getSignKey(secretKey))
				.build()
				.parseClaimsJws(authToken);
			return true;
		} catch (SignatureException ex) {
			log.error("Invalid JWT signature");
		} catch (MalformedJwtException ex) {
			log.error("Invalid JWT token");
		} catch (ExpiredJwtException ex) {
			log.error("Expired JWT token");
		} catch (UnsupportedJwtException ex) {
			log.error("Unsupported JWT token");
		} catch (IllegalArgumentException ex) {
			log.error("JWT claims string is empty");
		}
		return false;
	}

	public Claims getClaims(String token) {
		try {
			return Jwts.parserBuilder() // JwtParserBuilder 인스턴스 생성
				.setSigningKey(getSignKey(secretKey)) // JWT Signature 검증을 위한 SecretKey 설정
				.build() // Thread-Safe한 JwtParser를 반환하기 위해 build 호출
				.parseClaimsJws(token) // Claim(Payload) 파싱
				.getBody();
		} catch (ExpiredJwtException e) {
			// 만료된 토큰이어도 refresh token 검증 후 재발급할 수 있또록 claims 반환
			return e.getClaims();
		} catch (Exception e) {
			// 다른 예외인 경우 throw
			log.error("유효하지 않은 토큰입니다. {}", e.toString());
			throw new IllegalStateException("Access Token" + token + e.toString());
		}
	}

	public Authentication getAuthentication(String token) {
		String email = getClaims(token).getSubject();

		if (email == null){
			throw new IllegalStateException("Access token" + token + "권한 정보가 없는 토큰입니다.");
		}

		UserDetails userDetails = customUserDetailsService.loadUserByUsername(email);
		return new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
	}

	public Long getRemainingTime(String token) {
		Date expiration = getClaims(token).getExpiration();
		Date now = new Date();
		return expiration.getTime() - now.getTime();
	}

}
