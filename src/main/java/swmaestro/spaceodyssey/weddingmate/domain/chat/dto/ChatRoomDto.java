package swmaestro.spaceodyssey.weddingmate.domain.chat.dto;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatRoom;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ChatRoomDto implements Serializable {

	private static final long serialVersionUID = 6494678977089006639L;
	private static final String ROOM_NAME_CONNECTOR = "_";

	private String roomName;
	private String roomId;
	private String sender;
	private String receiver;
	private LocalDateTime lastMessageTime;

	// 쪽지방 생성
	public static ChatRoomDto create(String senderEmail, String receiverEmail) {
		return ChatRoomDto.builder()
			.roomId(UUID.randomUUID().toString())
			.roomName(senderEmail + ROOM_NAME_CONNECTOR + receiverEmail)
			.sender(senderEmail)
			.receiver(receiverEmail)
			.build();
	}

	@Builder
	public ChatRoomDto(String roomName, String roomId, String sender, String receiver) {
		this.roomName = roomName;
		this.roomId = roomId;
		this.sender = sender;
		this.receiver = receiver;
	}

	public ChatRoom toEntity() {
		return ChatRoom.builder()
			.roomName(this.roomName)
			.roomId(this.roomId)
			.sender(this.sender)
			.receiver(this.receiver)
			.build();
	}
}