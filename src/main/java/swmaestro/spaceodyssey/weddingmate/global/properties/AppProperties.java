package swmaestro.spaceodyssey.weddingmate.global.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Getter;
import lombok.Setter;

@Getter
@Configuration
@EnableConfigurationProperties
@ConfigurationProperties(prefix = "app")
public class AppProperties {

	private final Auth auth = new Auth();
	private final OAuth2 oAuth2 = new OAuth2();

	@Getter
	@Setter
	public static class Auth {
		private String tokenSecret;
		private String refreshCookieKey;
		private long tokenExpirationMsec;
	}

	@Getter
	public static final class OAuth2 {
		private String authorizationRedirectUris;
	}
}
