package swmaestro.spaceodyssey.weddingmate.domain.chat.dto;

import java.io.Serializable;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatMessage;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ChatMessageDto implements Serializable {

	private static final long serialVersionUID = 6494678977089006639L;

	private String roomId;
	private String sender;
	private String message;

	public ChatMessageDto(ChatMessage chatMessage) {
		this.sender = chatMessage.getSender();
		this.roomId = chatMessage.getRoomId();
		this.message = chatMessage.getMessage();
	}
}