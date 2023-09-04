package swmaestro.spaceodyssey.weddingmate.domain.chat.entity;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatRoomResDto;

@Getter
@NoArgsConstructor
@Entity
public class ChatRooms {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long chatRoomId;

	private String roomName;

	@Column(unique = true)
	private String roomId;

	private String sender;

	private String receiver;

	@OneToMany(mappedBy = "chatRooms", cascade = CascadeType.REMOVE)
	private List<ChatMessages> chatMessagesList = new ArrayList<>();

	private String lastMessage;

	private LocalDateTime lastMessageTime;

	@Builder
	public ChatRooms(String roomName, String roomId, String sender, String receiver) {
		this.roomName = roomName;
		this.roomId = roomId;
		this.sender = sender;
		this.receiver = receiver;
		this.lastMessageTime = LocalDateTime.now();
	}

	public ChatRoomResDto toChatRoomResDto() {
		return ChatRoomResDto.builder()
			.roomName(this.roomName)
			.roomId(this.roomId)
			.sender(this.sender)
			.receiver(this.receiver)
			.latestMessage(getLastMessageOrDefault())
			.lastMessageTime(this.lastMessageTime)
			.build();
	}

	public String getLastMessageOrDefault() {
		return this.chatMessagesList.isEmpty() ? "" : this.chatMessagesList.get(this.chatMessagesList.size() - 1).getMessage();
	}

	public void updateLastMessage(String lastMessage) {
		this.lastMessage = lastMessage;
	}
	public void updateLastMessageTime(LocalDateTime lastMessageTime) {
		this.lastMessageTime = lastMessageTime;
	}
}
