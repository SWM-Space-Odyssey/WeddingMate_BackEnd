package swmaestro.spaceodyssey.weddingmate.domain.category;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum CategoryEnum {
	PLANNER("플래너"), PORTFOLIO("분위기");

	private String value;
}
