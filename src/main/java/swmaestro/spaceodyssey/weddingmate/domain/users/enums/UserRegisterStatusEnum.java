package swmaestro.spaceodyssey.weddingmate.domain.users.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum UserRegisterStatusEnum {
	UNREGISTERED("미가입"),
	PLANNER("플래너"),
	CUSTOMER("예비부부");

	private String value;
}
