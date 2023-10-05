package swmaestro.spaceodyssey.weddingmate.global.config;

import java.util.Collections;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.handler.OAuth2AuthenticationFailureHandler;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.handler.OAuth2AuthenticationSuccessHandler;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.service.CookieAuthorizationRequestRepository;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.service.CustomOAuth2UserService;
import swmaestro.spaceodyssey.weddingmate.global.config.jwt.JwtAccessDeniedHandler;
import swmaestro.spaceodyssey.weddingmate.global.config.jwt.JwtAuthenticationEntryPoint;
import swmaestro.spaceodyssey.weddingmate.global.config.jwt.JwtAuthenticationFilter;
import swmaestro.spaceodyssey.weddingmate.global.config.jwt.JwtTokenProvider;

@EnableMethodSecurity(prePostEnabled = true)
@RequiredArgsConstructor
@EnableWebSecurity
@Configuration
public class SecurityConfig {

	private static final Long MAX_AGE_SECS = 3600L;

	// JWT
	private final JwtTokenProvider jwtTokenProvider;
	private final JwtAuthenticationEntryPoint jwtAuthenticationEntryPoint;
	private final JwtAuthenticationFilter jwtAuthenticationFilter;
	private final JwtAccessDeniedHandler jwtAccessDeniedHandler;

	// OAUTH2
	private final CustomOAuth2UserService customOAuth2UserService;
	private final OAuth2AuthenticationSuccessHandler authenticationSuccessHandler;
	private final OAuth2AuthenticationFailureHandler authenticationFailureHandler;

	// COOKIE
	private final CookieAuthorizationRequestRepository cookieAuthorizationRequestRepository;

	@Bean
	public BCryptPasswordEncoder passwordEncoder() {
		return new BCryptPasswordEncoder();
	}

	@Bean
	public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
		http.csrf(AbstractHttpConfigurer::disable)
			.cors(conf -> conf.configurationSource(corsConfigurationSource()))
			.sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
			.formLogin(AbstractHttpConfigurer::disable)
			.httpBasic(AbstractHttpConfigurer::disable);

		// 요청에 대한 권한 설정
		http.authorizeHttpRequests(auth ->
			auth
				.requestMatchers("/login/**", "/oauth2/**").permitAll()
				.requestMatchers("/stomp/**").permitAll()
				.requestMatchers("/api/v1/token/**").permitAll()
				.requestMatchers("/v3/api-docs/**").permitAll()
				.requestMatchers("swagger-ui/**").permitAll()
				.anyRequest().authenticated()
		);

		// OAuth2
		http.oauth2Login(oauth ->
			oauth
				//  인증 요청을 쿠키에 임시 보관하는 리포지토리에 대한 설정으로, 인증 후 프론트에 redirect할 url이 저장되어 있음
				.authorizationEndpoint(authorizationEndpointConfig ->
					authorizationEndpointConfig.authorizationRequestRepository(cookieAuthorizationRequestRepository)
				)
				// 로그인 성공 시 사용자 정보 가져올 때의 설정 관리
				.userInfoEndpoint(userInfoEndpointConfig ->
					userInfoEndpointConfig.userService(customOAuth2UserService)
				)
				.successHandler(authenticationSuccessHandler)
				.failureHandler(authenticationFailureHandler)
		);

		http.exceptionHandling(exhandling ->
			exhandling
				.authenticationEntryPoint(jwtAuthenticationEntryPoint)   // 401
				.accessDeniedHandler(jwtAccessDeniedHandler)            // 403
		);

		// Jwt filter 설정
		http.addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);

		return http.build();
	}

	public CorsConfigurationSource corsConfigurationSource() {
		CorsConfiguration configuration = new CorsConfiguration();
		configuration.addAllowedHeader("*");
		configuration.addAllowedMethod("*"); // GET, POST, PUT, DELETE (javascript 요청 허용)
		configuration.addAllowedOrigin("http://weddingmate-fe-bucket.s3-website.ap-northeast-2.amazonaws.com");
		configuration.addAllowedOrigin("http://localhost:5173");
		configuration.addAllowedOrigin("https://weddingmate.co.kr");
		configuration.addAllowedOrigin("https://dev.weddingmate.co.kr");
		configuration.addAllowedOrigin("https://server.weddingmate.co.kr");
		configuration.addAllowedOrigin("https://api.weddingmate.co.kr");
		configuration.setAllowCredentials(true); // 클라이언트에서 쿠키 요청 허용
		configuration.setAllowedOriginPatterns(Collections.singletonList(
			"*")); // 모든 IP 주소 허용 (프론트엔드 IP, react만 허용) 핸드폰은 js 요청을 하지 않고 java나 swift 쓰기 때문에 cors에 안 걸림
		configuration.setMaxAge(MAX_AGE_SECS);

		UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
		source.registerCorsConfiguration("/**", configuration); // 모든 주소 요청에 대해 해당 설정 적용

		return source;
	}
}