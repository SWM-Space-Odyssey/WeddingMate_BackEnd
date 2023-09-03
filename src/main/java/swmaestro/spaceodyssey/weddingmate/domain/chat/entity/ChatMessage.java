package swmaestro.spaceodyssey.weddingmate.domain.chat.entity;

import java.time.LocalDateTime;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageResDto;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ChatMessage {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long chatMessageId;

	private String roomId;

	private String sender;

	private String message;

	private LocalDateTime lastMessageTime;

	@ManyToOne
	@JoinColumn(name = "roomId", referencedColumnName = "roomId", insertable = false, updatable = false)
	private ChatRoom chatRoom;

	@Builder
	public ChatMessage(String sender, String roomId, String message) {
		this.sender = sender;
		this.roomId = roomId;
		this.message = message;
		this.lastMessageTime = LocalDateTime.now();
	}

	public ChatMessageResDto toChatMessageResDto(){
		return ChatMessageResDto.builder()
			.roomId(this.roomId)
			.sender(this.sender)
			.message(this.message)
			.lastMessageTime(this.lastMessageTime)
			.build();
	}

}
