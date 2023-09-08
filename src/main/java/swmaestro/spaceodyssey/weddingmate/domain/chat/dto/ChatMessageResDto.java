package swmaestro.spaceodyssey.weddingmate.domain.chat.dto;

import java.io.Serializable;
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
public class ChatMessageResDto implements Serializable {

	private static final long serialVersionUID = 6494678977089006639L;

	private String roomId;
	private String sender;
	private String message;

	@JsonSerialize(using = LocalDateTimeSerializer.class)
	@JsonDeserialize(using = LocalDateTimeDeserializer.class)
	private LocalDateTime lastMessageTime;

	@Builder
	public ChatMessageResDto(String roomId, String sender, String message, LocalDateTime lastMessageTime) {
		this.roomId = roomId;
		this.sender = sender;
		this.message = message;
		this.lastMessageTime = lastMessageTime;
	}
}