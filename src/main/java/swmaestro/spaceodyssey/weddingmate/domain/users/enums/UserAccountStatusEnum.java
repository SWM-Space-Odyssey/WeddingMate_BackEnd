package swmaestro.spaceodyssey.weddingmate.domain.users.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum UserAccountStatusEnum {
	NORMAL("정상"),
	SUSPENDED("정지"),
	WITHDRAW("탈퇴"),
	BANNED("영구정지");

	private String value;
}
