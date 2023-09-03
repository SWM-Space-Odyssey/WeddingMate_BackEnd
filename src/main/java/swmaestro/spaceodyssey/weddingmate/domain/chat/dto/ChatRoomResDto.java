package swmaestro.spaceodyssey.weddingmate.domain.chat.dto;

import java.time.LocalDateTime;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatRoom;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ChatRoomResDto {
	private String roomName;
	private String roomId;
	private String sender;
	private String receiver;
	private String latestMessage;
	private LocalDateTime lastMessageTime;

	// 쪽지방 생성
	public ChatRoomResDto(ChatRoom entity) {
		this.roomName = entity.getRoomName();
		this.roomId = entity.getRoomId();
		this.sender = entity.getSender();
		this.receiver = entity.getReceiver();
		this.latestMessage = entity.getChatMessageList().get(0).getMessage();
		this.lastMessageTime = entity.getLastMessageTime();
	}

	// 사용자 관련 쪽지방 전체 조회
	@Builder
	public ChatRoomResDto(String roomName, String roomId, String sender, String receiver,
			String latestMessage, LocalDateTime lastMessageTime) {
		this.roomName = roomName;
		this.roomId = roomId;
		this.sender = sender;
		this.receiver = receiver;
		this.latestMessage = latestMessage;
		this.lastMessageTime = lastMessageTime;
	}
}
