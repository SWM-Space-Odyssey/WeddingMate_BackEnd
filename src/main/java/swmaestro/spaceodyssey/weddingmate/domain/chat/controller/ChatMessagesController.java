package swmaestro.spaceodyssey.weddingmate.domain.chat.controller;

import java.util.List;

import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Hidden;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageResDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.pubsub.RedisPublisher;
import swmaestro.spaceodyssey.weddingmate.domain.chat.service.ChatMessagesService;
import swmaestro.spaceodyssey.weddingmate.domain.chat.service.ChatRoomsService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Hidden
@RequiredArgsConstructor
@RestController
public class ChatMessagesController {

	private final RedisPublisher redisPublisher;
	private final ChatRoomsService chatRoomService;
	private final ChatMessagesService chatMessageService;

	// 대화 & 대화 저장
	@MessageMapping("/chat/message") // /pub/chat/message로 요청해야 함
	public void sendMessage(@RequestBody ChatMessageDto reqDto) {
		// 클라이언트의 쪽지방(topic) 입장, 대화를 위해 리스너와 연동
		chatRoomService.enterChatRoom(reqDto.getRoomId());
		// Websocket에 발행된 메시지를 redis로 ㅂ라행
		// 해당 쪽지방을 구독한 클라이언트에게 메시지 전송
		redisPublisher.publish(chatRoomService.getTopic(reqDto.getRoomId()), reqDto);
		// DB & redis에 대화 저장
		chatMessageService.saveMessage(reqDto);
	}

	@GetMapping("/api/v1/chat/message/{roomId}")
	public ApiResponse<Object> getMessageList(@PathVariable String roomId) {
		List<ChatMessageResDto> messageList = chatMessageService.getChatMessageList(roomId);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(messageList)
			.build();
	}
}
