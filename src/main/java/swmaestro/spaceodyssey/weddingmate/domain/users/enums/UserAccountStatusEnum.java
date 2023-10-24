package swmaestro.spaceodyssey.weddingmate.domain.users.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum UserAccountStatusEnum {
	NORMAL("정상"),
	NON_ELIGIBLE("비대상"),
	SUSPENDED("정지"),
	WITHDRAW("탈퇴"),
	BANNED("재가입불가");

	private String value;
}
