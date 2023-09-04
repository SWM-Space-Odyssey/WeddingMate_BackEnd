package swmaestro.spaceodyssey.weddingmate.domain.chat.pubsub;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageDto;

@RequiredArgsConstructor
@Service
public class RedisPublisher {

	private final RedisTemplate<String, Object> redisTemplate;

	// Redis Topic에 메시지 발행
	// 메시지 발행 후, 대기 중이던 redis subscriber가 메시지 처리
	public void publish(ChannelTopic topic, ChatMessageDto message) {
		redisTemplate.convertAndSend(topic.getTopic(), message);
	}
}
