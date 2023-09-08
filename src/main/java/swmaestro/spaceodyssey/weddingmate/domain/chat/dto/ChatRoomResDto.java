package swmaestro.spaceodyssey.weddingmate.domain.chat.dto;

import java.time.LocalDateTime;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ChatRoomResDto {
	private String roomName;
	private String roomId;
	private String sender;
	private String receiver;
	private String latestMessage;

	@JsonSerialize(using = LocalDateTimeSerializer.class)
	@JsonDeserialize(using = LocalDateTimeDeserializer.class)
	private LocalDateTime lastMessageTime;

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
