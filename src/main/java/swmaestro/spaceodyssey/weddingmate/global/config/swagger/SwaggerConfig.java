package swmaestro.spaceodyssey.weddingmate.global.config.swagger;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;

@Configuration
public class SwaggerConfig {

	private static final String BEARER_TOKEN_PREFIX = "Bearer";
	private static final String JWT_SCHEME_NAME = "JWT";

	@Bean
	public OpenAPI openAPI() {

		// JWT 관련 인증 설정
		SecurityRequirement securityRequirement = new SecurityRequirement().addList(JWT_SCHEME_NAME); // 헤더에 토큰 포함
		Components components = new Components().addSecuritySchemes(JWT_SCHEME_NAME, new SecurityScheme()
			.name(JWT_SCHEME_NAME)
			.type(SecurityScheme.Type.HTTP)
			.scheme(BEARER_TOKEN_PREFIX)
			.bearerFormat(JWT_SCHEME_NAME)
		);

		return new OpenAPI()
			.info(apiInfo())
			.addServersItem(new Server().url("/"))
			.addSecurityItem(securityRequirement)
			.components(components);
	}

	private Info apiInfo() {
		return new Info()
			.title("Weddingmate")
			.description("Weddingmate API 공식 문서")
			.version("2.0.0");
	}
}
