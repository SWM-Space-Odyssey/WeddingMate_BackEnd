package swmaestro.spaceodyssey.weddingmate.domain.like.enums;

import lombok.Getter;

@Getter
public enum LikeEnum {
	portfolio("portfolioLikeImpl"),
	item("itemLikeImpl"),
	planner("plannerLikeImpl"),
	company("companyLikeImpl");

	private final String serviceName;

	LikeEnum(String serviceName) {
		this.serviceName = serviceName;
	}
}

