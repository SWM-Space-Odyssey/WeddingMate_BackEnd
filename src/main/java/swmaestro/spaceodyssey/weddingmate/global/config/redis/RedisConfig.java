package swmaestro.spaceodyssey.weddingmate.global.config.redis;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.serializer.Jackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageDto;

@Configuration
public class RedisConfig {

	@Value("${spring.data.redis.host}")
	private String host;

	@Value("${spring.data.redis.port}")
	private int port;

	@Bean
	public RedisConnectionFactory redisConnectionFactory() {
		RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
		redisStandaloneConfiguration.setHostName(host);
		redisStandaloneConfiguration.setPort(port);

		return new LettuceConnectionFactory(redisStandaloneConfiguration);
	}

	/**
	 * redis pub/sub 메시지를 처리하는 listener 설정
	 */
	@Bean
	public RedisMessageListenerContainer redisMessageListener(RedisConnectionFactory connectionFactory) {
		RedisMessageListenerContainer container = new RedisMessageListenerContainer();
		container.setConnectionFactory(connectionFactory);
		return container;
	}

	/**
	 * 어플리케이션에서 사용할 redisTemplate 설정
	 */
	@Bean
	public RedisTemplate<String, Object> redisTemplate(RedisConnectionFactory connectionFactory) {
		RedisTemplate<String, Object> redisTemplate = new RedisTemplate<>();
		redisTemplate.setConnectionFactory(connectionFactory);
		redisTemplate.setKeySerializer(new StringRedisSerializer());
		// Java 객체를 JSON 형식으로 직렬화하고 역직렬화하는 데 사용
		// Jackson2JsonRedisSerializer 생성자의 타겟 클래스를 ChatMessage.class로 변경하여 제대로된 직렬화와 역직렬화가 이루어지도록 한다
		redisTemplate.setValueSerializer(new Jackson2JsonRedisSerializer<>(String.class));
		return redisTemplate;
	}

	/*
	* Redis에 메시지 내역을 저장하기 위한 redisTemplate 설정
	*/
	@Bean
	public RedisTemplate<String, ChatMessageDto> redisMessageTemplate(RedisConnectionFactory connectionFactory) {
		RedisTemplate<String, ChatMessageDto> redisMessageTemplate = new RedisTemplate<>();
		redisMessageTemplate.setConnectionFactory(connectionFactory);
		redisMessageTemplate.setKeySerializer(new StringRedisSerializer());
		redisMessageTemplate.setValueSerializer(new Jackson2JsonRedisSerializer<>(ChatMessageDto.class));

		return redisMessageTemplate;
	}
}
