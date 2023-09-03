package swmaestro.spaceodyssey.weddingmate.domain.chat.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatRoom;

public interface ChatRoomRepository extends JpaRepository<ChatRoom, Long> {
	@Query("SELECT cr FROM ChatRoom cr WHERE cr.sender = :userEmail OR cr.receiver = :userEmail")
	List<ChatRoom> findByUserEmail(@Param("userEmail") String userEmail);
	ChatRoom findBySenderAndReceiver(String sender, String receiver);
	Optional<ChatRoom> findByRoomId(String roomId);
}
