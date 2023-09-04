package swmaestro.spaceodyssey.weddingmate.global.exception.chat;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class ChatRoomNotFoundException extends RuntimeException {
	public ChatRoomNotFoundException() {
		super(CHATROOM_NOTFOUND);
	}
}