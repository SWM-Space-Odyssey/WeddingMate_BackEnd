package swmaestro.spaceodyssey.weddingmate.domain.chat.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatMessage;

public interface ChatMessageRepository extends JpaRepository<ChatMessage, Long> {
	List<ChatMessage> findByRoomIdOrderByLastMessageTimeAsc(String roomId);
	ChatMessage findTopByRoomIdOrderByLastMessageTimeDesc(String roomId);
}
