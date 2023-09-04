package swmaestro.spaceodyssey.weddingmate.global.exception.chat;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.FORBIDDEN)
public class ChatRoomNotAuthorizedException extends RuntimeException{
	public ChatRoomNotAuthorizedException() {
		super(CHATROOM_NOT_AUTHORIZED);
	}
}
