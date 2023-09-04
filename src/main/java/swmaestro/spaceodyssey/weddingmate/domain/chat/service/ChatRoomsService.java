package swmaestro.spaceodyssey.weddingmate.domain.chat.service;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatRoomResDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatRoomDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatMessages;
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatRooms;
import swmaestro.spaceodyssey.weddingmate.domain.chat.pubsub.RedisSubscriber;
import swmaestro.spaceodyssey.weddingmate.domain.chat.repository.ChatRoomsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.UsersService;
import swmaestro.spaceodyssey.weddingmate.global.exception.chat.ChatRoomNotAuthorizedException;
import swmaestro.spaceodyssey.weddingmate.global.exception.chat.ChatRoomNotFoundException;

@Slf4j
@Transactional
@Service
@RequiredArgsConstructor
public class ChatRoomsService {
	private final ChatRoomsRepository chatRoomRepository;
	private final UsersService usersService;

	// 쪽지방(topic)에 발행되는 메시지 처리하는 리스너
	private final RedisMessageListenerContainer redisMessageListenerContainer;

	// 구독 처리 서비스
	private final RedisSubscriber redisSubscriber;

	// 1. redis
	private static final String CHAT_ROOM_PREFIX = "CHAT_ROOM";
	private final RedisTemplate<String, Object> redisTemplate;
	private HashOperations<String, String, ChatRoomDto> opsHashChatRoom;

	// 2. 쪽지방 대화 메시지 발행을 위한 redis 쪽지방(topic) 정보
	private Map<String, ChannelTopic> topicMap;

	// 3. Redis Hash 초기화
	@PostConstruct
	private void init() {
		opsHashChatRoom = redisTemplate.opsForHash();
		topicMap = new HashMap<>();
	}

	/*================== Message 관련 함수 ==================*/
	public void enterChatRoom(String roomId) {
		topicMap.computeIfAbsent(roomId, key -> {
			ChannelTopic topic = new ChannelTopic(key);
			redisMessageListenerContainer.addMessageListener(redisSubscriber, topic);
			return topic;
		});
	}

	public ChannelTopic getTopic(String roomId){
		return topicMap.get(roomId);
	}

	@Transactional
	public void updateChatRoomLastMessageAndTime(String roomId, ChatMessages chatMessages){
		ChatRooms chatRooms = findByRoomId(roomId);
		chatRooms.updateLastMessage(chatMessages.getMessage());
		chatRooms.updateLastMessageTime(chatMessages.getLastMessageTime());
	}

	/*================== Room CRUD 관련 함수 ==================*/
	public ChatRoomResDto createRoom(Users users, ChatMessageReqDto reqDto) {
		String senderEmail = users.getEmail();
		String receiverEmail = usersService.findUserByNickname(reqDto.getReceiver()).getEmail();
		ChatRooms chatRooms = findBySenderAndReceiver(senderEmail, receiverEmail);

		// 1) 처음 쪽지방 생성하는 경우
		if (chatRooms == null) {
			ChatRoomDto chatRoomDto = ChatRoomDto.create(senderEmail, receiverEmail);
			opsHashChatRoom.put(CHAT_ROOM_PREFIX, chatRoomDto.getRoomId(),chatRoomDto);
			chatRooms = chatRoomRepository.save(chatRoomDto.toEntity());
		}

		return chatRooms.toChatRoomResDto();
	}

	public List<ChatRoomResDto> findAllRoomByUser(Users users){
		List<ChatRooms> chatRoomsList = findByUserEmail(users.getEmail());

		return chatRoomsList.stream()
			.filter(chatRooms -> {
				String latestMessage = chatRooms.getLastMessage();
				return latestMessage != null;
			})
			.map(chatRooms -> {
				String roomName = createRoomName(users, chatRooms);

				return ChatRoomResDto.builder()
						.roomName(roomName)
						.roomId(chatRooms.getRoomId())
						.sender(chatRooms.getSender())
						.receiver(chatRooms.getReceiver())
						.latestMessage(chatRooms.getLastMessage())
						.lastMessageTime(chatRooms.getLastMessageTime())
						.build();
			})
			.toList();
	}

	public ChatRoomResDto findRoomByRoomId(String roomId) {
		ChatRooms chatRooms = findByRoomId(roomId);

		return chatRooms.toChatRoomResDto();
	}

	public String deleteRoom(Users users, String roomId) {
		ChatRooms chatRooms = findByRoomId(roomId);

		checkUserIsChatRoomOwner(users, chatRooms);

		opsHashChatRoom.delete(CHAT_ROOM_PREFIX, roomId);
		chatRoomRepository.delete(chatRooms);

		return CHATROOM_DELETE_SUCCESS;
	}

	/*================== 편의성 함수 ==================*/
	public String createRoomName(Users users, ChatRooms chatRooms) {
		String email = users.getEmail().equals(chatRooms.getSender()) ? chatRooms.getReceiver() : chatRooms.getSender();
		return usersService.findUserByEmail(email).getNickname();
	}

	public void checkUserIsChatRoomOwner(Users users, ChatRooms chatRooms) {
		if (!(users.getEmail().equals(chatRooms.getSender()) || users.getEmail().equals(chatRooms.getReceiver()))) {
			throw new ChatRoomNotAuthorizedException();
		}
	}

	/*================== Repository 접근 ==================*/
	@Transactional(readOnly = true)
	public ChatRooms findBySenderAndReceiver(String senderEmail, String receiverEmail) {
		return chatRoomRepository.findBySenderAndReceiver(senderEmail, receiverEmail);
	}

	@Transactional(readOnly = true)
	public List<ChatRooms> findByUserEmail(String userEmail) {
		return chatRoomRepository.findByUserEmail(userEmail);
	}

	@Transactional(readOnly = true)
	public ChatRooms findByRoomId(String roomId) {
		return chatRoomRepository.findByRoomId(roomId)
			.orElseThrow(ChatRoomNotFoundException::new);
	}
}
