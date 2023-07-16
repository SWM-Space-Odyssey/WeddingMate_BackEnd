package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum UserEnum {
	NORMAL("정상"),
	SUSPENDED("정지"),
	WITHDRAW("탈퇴"),
	BANNED("재가입불가");

	private String value;
}
