package swmaestro.spaceodyssey.weddingmate.global.config.jwt;

import java.security.Key;
import java.util.Date;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.UnsupportedJwtException;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import io.jsonwebtoken.security.SignatureException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.UserPrincipal;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.service.CustomUserDetailsService;
import swmaestro.spaceodyssey.weddingmate.global.properties.AppProperties;

@Component
@Slf4j
@RequiredArgsConstructor
public class JwtTokenProvider {

	private final AppProperties appProperties;
	private final CustomUserDetailsService customUserDetailsService;

	public String createToken(Authentication authentication) {
		UserPrincipal userPrincipal = (UserPrincipal)authentication.getPrincipal();

		Date now = new Date();
		Date expiredDate = new Date(now.getTime() + appProperties.getAuth().getTokenExpirationMsec());

		byte[] keyBytes = Decoders.BASE64.decode(appProperties.getAuth().getTokenSecret());
		Key key = Keys.hmacShaKeyFor(keyBytes);

		return Jwts.builder()
			.setSubject(Long.toString(userPrincipal.getId()))
			.setIssuedAt(new Date())
			.setExpiration(expiredDate)
			.signWith(key, SignatureAlgorithm.HS256)
			.compact();
	}

	public Long getUserIdFromToken(String token) {
		byte[] keyBytes = Decoders.BASE64.decode(appProperties.getAuth().getTokenSecret());
		Key key = Keys.hmacShaKeyFor(keyBytes);

		Claims claims = Jwts.parserBuilder()
			.setSigningKey(key)
			.build()
			.parseClaimsJws(token)
			.getBody();

		return Long.parseLong(claims.getSubject());
	}

	public boolean validateToken(String authToken) {
		byte[] keyBytes = Decoders.BASE64.decode(appProperties.getAuth().getTokenSecret());
		Key key = Keys.hmacShaKeyFor(keyBytes);

		try {
			Jwts.parserBuilder()
				.setSigningKey(key)
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

	public Authentication getAuthentication(String token) {
		Long userId = getUserIdFromToken(token);
		UserDetails userDetails = customUserDetailsService.loadUserById(userId);
		return new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
	}

}
