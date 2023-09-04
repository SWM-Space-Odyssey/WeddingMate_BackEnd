package swmaestro.spaceodyssey.weddingmate.domain.chat.dto;

import java.io.Serializable;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ChatMessageReqDto implements Serializable {

	private static final long serialVersionUID = 990913L;

	private String receiver; // receiver nickname
}
