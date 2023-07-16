package swmaestro.spaceodyssey.weddingmate.global.properties;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Getter;
import lombok.Setter;

@Getter
@Configuration
@EnableConfigurationProperties
@ConfigurationProperties(prefix = "spring.security.oauth2.client")
public class OAuth2ClientProperties {

	private final Map<String, Provider> provider = new HashMap<>();

	private final Map<String, Registration> registration = new HashMap<>();

	@Getter
	@Setter
	public static class Registration {
		private String provider;
		private String clientId;
		private String clientSecret;
		private String clientAuthenticationMethod;
		private String authorizationGrantType;
		private String redirectUri;
		private Set<String> scope;
		private String clientName;
	}

	@Getter
	@Setter
	public static class Provider {
		private String authorizationUri;
		private String tokenUri;
		private String userInfoUri;
		private String userNameAttribute;
	}
}
