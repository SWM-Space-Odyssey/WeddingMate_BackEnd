package swmaestro.spaceodyssey.weddingmate.domain.chat.service;

import java.util.List;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.Jackson2JsonRedisSerializer;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageResDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatMessage;
import swmaestro.spaceodyssey.weddingmate.domain.chat.repository.ChatMessageRepository;

@Slf4j
@Transactional
@Service
@RequiredArgsConstructor
public class ChatMessageService {
	private final RedisTemplate<String, ChatMessageResDto> redisMessageTemplate;
	private final ChatMessageRepository chatMessageRepository;
	private final ChatRoomService chatRoomService;

	public void saveMessage(ChatMessageDto dto) {
		// DB 저장
		ChatMessage chatMessage = ChatMessage.builder()
			.roomId(dto.getRoomId())
			.sender(dto.getSender())
			.message(dto.getMessage())
			.build();
		chatMessageRepository.save(chatMessage);

		ChatMessageResDto chatMessageResDto = chatMessage.toChatMessageResDto();

		// 직렬화
		redisMessageTemplate.setValueSerializer(new Jackson2JsonRedisSerializer<>(ChatMessageResDto.class));

		// redis 저장 (최신일수록 오른쪽)
		redisMessageTemplate.opsForList().rightPush(chatMessageResDto.getRoomId(), chatMessageResDto);

		// 채팅방 lastMessageTime 업데이트
		chatRoomService.updateChatRoomLastMessageAndTime(chatMessageResDto.getRoomId(), chatMessage);

		// 1시간마다 redis에서 메시지 삭제
		//redisMessageTemplate.expire(dto.getRoomId(), 1, TimeUnit.HOURS);
	}

	public List<ChatMessageResDto> getChatMessageList(String roomId) {
		// TODO : 메시지 개수 정해서 가져오게 하기
		return redisMessageTemplate.opsForList().range(roomId, 0, -1);
	}
}
