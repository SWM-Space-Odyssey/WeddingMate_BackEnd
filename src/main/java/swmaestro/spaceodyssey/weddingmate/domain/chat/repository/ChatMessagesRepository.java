package swmaestro.spaceodyssey.weddingmate.domain.chat.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatMessages;

public interface ChatMessagesRepository extends JpaRepository<ChatMessages, Long> {
	List<ChatMessages> findByRoomIdOrderByLastMessageTimeAsc(String roomId);
	ChatMessages findTopByRoomIdOrderByLastMessageTimeDesc(String roomId);
}
