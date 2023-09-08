package swmaestro.spaceodyssey.weddingmate.global.config.stomp;

import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.config.annotation.EnableWebSocketMessageBroker;
import org.springframework.web.socket.config.annotation.StompEndpointRegistry;
import org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer;

import lombok.RequiredArgsConstructor;

@Configuration
@EnableWebSocketMessageBroker
@RequiredArgsConstructor
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {

	@Override
	public void configureMessageBroker(MessageBrokerRegistry config) {
		config.enableSimpleBroker("/sub"); // 메시지를 구독하는 요청의 prefix는 sub
		config.setApplicationDestinationPrefixes("/pub"); // 메시지를 발행하는 요청의 prefix는 pub
	}

	@Override
	public void registerStompEndpoints(StompEndpointRegistry registry) {
		// socketJs 없을 때 : ws://localhost:8080/stomp
		// 개발서버 접속 주소 : http://localhost:8080/stomp
		registry.addEndpoint("/stomp").setAllowedOriginPatterns("*").withSockJS();
	}
}