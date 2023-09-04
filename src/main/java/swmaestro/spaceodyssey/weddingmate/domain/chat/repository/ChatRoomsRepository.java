package swmaestro.spaceodyssey.weddingmate.domain.chat.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import swmaestro.spaceodyssey.weddingmate.domain.chat.entity.ChatRooms;

public interface ChatRoomsRepository extends JpaRepository<ChatRooms, Long> {
	@Query("SELECT cr FROM ChatRooms cr WHERE cr.sender = :userEmail OR cr.receiver = :userEmail")
	List<ChatRooms> findByUserEmail(@Param("userEmail") String userEmail);
	ChatRooms findBySenderAndReceiver(String sender, String receiver);
	Optional<ChatRooms> findByRoomId(String roomId);
}
