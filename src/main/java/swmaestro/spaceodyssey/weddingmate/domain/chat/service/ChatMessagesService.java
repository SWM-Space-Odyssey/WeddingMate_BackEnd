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
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatMessages;
import swmaestro.spaceodyssey.weddingmate.domain.chat.repository.ChatMessagesRepository;

@Slf4j
@Transactional
@Service
@RequiredArgsConstructor
public class ChatMessagesService {
	private final RedisTemplate<String, ChatMessageResDto> redisMessageTemplate;
	private final ChatMessagesRepository chatMessageRepository;
	private final ChatRoomsService chatRoomService;

	public void saveMessage(ChatMessageDto dto) {
		// DB 저장
		ChatMessages chatMessages = ChatMessages.builder()
			.roomId(dto.getRoomId())
			.sender(dto.getSender())
			.message(dto.getMessage())
			.build();
		chatMessageRepository.save(chatMessages);

		ChatMessageResDto chatMessageResDto = chatMessages.toChatMessageResDto();

		// 직렬화
		redisMessageTemplate.setValueSerializer(new Jackson2JsonRedisSerializer<>(ChatMessageResDto.class));

		// redis 저장 (최신일수록 오른쪽)
		redisMessageTemplate.opsForList().rightPush(chatMessageResDto.getRoomId(), chatMessageResDto);

		// 채팅방 lastMessageTime 업데이트
		chatRoomService.updateChatRoomLastMessageAndTime(chatMessageResDto.getRoomId(), chatMessages);

		// 1시간마다 redis에서 메시지 삭제
		//redisMessageTemplate.expire(dto.getRoomId(), 1, TimeUnit.HOURS);
	}

	public List<ChatMessageResDto> getChatMessageList(String roomId) {
		// TODO : 메시지 개수 정해서 가져오게 하기
		return redisMessageTemplate.opsForList().range(roomId, 0, -1);
	}
}
