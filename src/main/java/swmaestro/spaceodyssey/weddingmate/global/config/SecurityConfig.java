package swmaestro.spaceodyssey.weddingmate.global.config;

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
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.handler.OAuth2AuthenticationSuccessHandler;
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
				.anyRequest().authenticated());

		// OAuth2
		http.oauth2Login(oauth ->
			oauth
				.userInfoEndpoint(userInfoEndpointConfig ->  // 로그인 성공 시 사용자 정보 가져올 때의 설정 관리
					userInfoEndpointConfig.userService(customOAuth2UserService)
				)
				.successHandler(authenticationSuccessHandler)
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
		configuration.addAllowedOriginPattern("*"); // 모든 IP 주소 허용 (프론트엔드 IP, react만 허용) 핸드폰은 js 요청을 하지 않고 java나 swift 쓰기 때문에 cors에 안 걸림
		configuration.addAllowedOrigin("http://weddingmate-fe-bucket.s3-website.ap-northeast-2.amazonaws.com");
		configuration.addAllowedOrigin("http://localhost:5173");
		configuration.addAllowedOrigin("http://weddingmate.co.kr");
		configuration.setAllowCredentials(true); // 클라이언트에서 쿠키 요청 허용
		configuration.setMaxAge(MAX_AGE_SECS);

		UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
		source.registerCorsConfiguration("/**", configuration); // 모든 주소 요청에 대해 해당 설정 적용

		return source;
	}
}
