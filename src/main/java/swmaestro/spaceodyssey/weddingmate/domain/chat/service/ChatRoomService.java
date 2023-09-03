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
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatMessage;
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatRoom;
import swmaestro.spaceodyssey.weddingmate.domain.chat.pubsub.RedisSubscriber;
import swmaestro.spaceodyssey.weddingmate.domain.chat.repository.ChatRoomRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.UsersService;
import swmaestro.spaceodyssey.weddingmate.global.exception.chat.ChatRoomNotAuthorizedException;
import swmaestro.spaceodyssey.weddingmate.global.exception.chat.ChatRoomNotFoundException;

@Slf4j
@Transactional
@Service
@RequiredArgsConstructor
public class ChatRoomService {
	private final ChatRoomRepository chatRoomRepository;
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
	public void updateChatRoomLastMessageAndTime(String roomId, ChatMessage chatMessage){
		ChatRoom chatRoom = findByRoomId(roomId);
		chatRoom.updateLastMessage(chatMessage.getMessage());
		chatRoom.updateLastMessageTime(chatMessage.getLastMessageTime());
	}

	/*================== Room CRUD 관련 함수 ==================*/
	public ChatRoomResDto createRoom(Users users, ChatMessageReqDto reqDto) {
		String senderEmail = users.getEmail();
		String receiverEmail = usersService.findUserByNickname(reqDto.getReceiver()).getEmail();
		ChatRoom chatRoom = findBySenderAndReceiver(senderEmail, receiverEmail);

		// 1) 처음 쪽지방 생성하는 경우
		if (chatRoom == null) {
			ChatRoomDto chatRoomDto = ChatRoomDto.create(senderEmail, receiverEmail);
			opsHashChatRoom.put(CHAT_ROOM_PREFIX, chatRoomDto.getRoomId(),chatRoomDto);
			chatRoom = chatRoomRepository.save(chatRoomDto.toEntity());
		}

		return chatRoom.toChatRoomResDto();
	}

	public List<ChatRoomResDto> findAllRoomByUser(Users users){
		List<ChatRoom> chatRoomList = findByUserEmail(users.getEmail());

		return chatRoomList.stream()
			.filter(chatRoom -> {
				String latestMessage = chatRoom.getLastMessage();
				return latestMessage != null;
			})
			.map(chatRoom -> {
				String roomName = createRoomName(users, chatRoom);

				return ChatRoomResDto.builder()
						.roomName(roomName)
						.roomId(chatRoom.getRoomId())
						.sender(chatRoom.getSender())
						.receiver(chatRoom.getReceiver())
						.latestMessage(chatRoom.getLastMessage())
						.lastMessageTime(chatRoom.getLastMessageTime())
						.build();
			})
			.toList();
	}

	public ChatRoomResDto findRoomByRoomId(String roomId) {
		ChatRoom chatRoom = findByRoomId(roomId);

		return chatRoom.toChatRoomResDto();
	}

	public String deleteRoom(Users users, String roomId) {
		ChatRoom chatRoom = findByRoomId(roomId);

		checkUserIsChatRoomOwner(users, chatRoom);

		opsHashChatRoom.delete(CHAT_ROOM_PREFIX, roomId);
		chatRoomRepository.delete(chatRoom);

		return CHATROOM_DELETE_SUCCESS;
	}

	/*================== 편의성 함수 ==================*/
	public String createRoomName(Users users, ChatRoom chatRoom) {
		String email = users.getEmail().equals(chatRoom.getSender()) ? chatRoom.getReceiver() : chatRoom.getSender();
		return usersService.findUserByEmail(email).getNickname();
	}

	public void checkUserIsChatRoomOwner(Users users, ChatRoom chatRoom) {
		if (!(users.getEmail().equals(chatRoom.getSender()) || users.getEmail().equals(chatRoom.getReceiver()))) {
			throw new ChatRoomNotAuthorizedException();
		}
	}

	/*================== Repository 접근 ==================*/
	@Transactional(readOnly = true)
	public ChatRoom findBySenderAndReceiver(String senderEmail, String receiverEmail) {
		return chatRoomRepository.findBySenderAndReceiver(senderEmail, receiverEmail);
	}

	@Transactional(readOnly = true)
	public List<ChatRoom> findByUserEmail(String userEmail) {
		return chatRoomRepository.findByUserEmail(userEmail);
	}

	@Transactional(readOnly = true)
	public ChatRoom findByRoomId(String roomId) {
		return chatRoomRepository.findByRoomId(roomId)
			.orElseThrow(ChatRoomNotFoundException::new);
	}
}
