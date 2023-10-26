package swmaestro.spaceodyssey.weddingmate.domain.users.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum SuspensionPeriodEnum {
	FIRST_BLOCK(1, 14),
	SECOND_BLOCK(2, 30),

	private final int blockCnt;
	private final int days;

	public static SuspensionPeriodEnum getByBlockCnt(int blockCnt) {
		for (SuspensionPeriodEnum period : values()) {
			if (period.blockCnt == blockCnt) {
				return period;
			}
		}
		return null;
	}
}
