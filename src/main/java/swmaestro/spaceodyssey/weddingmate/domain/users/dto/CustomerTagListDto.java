package swmaestro.spaceodyssey.weddingmate.domain.users.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class CustomerTagListDto {
	private String portfolioTagList;

	private String plannerTagList;

	private String dressTagList;

	private String studioTypeTagList;

	private String studioFocusTagList;

	private String makeupTagList;

	@Builder
	public CustomerTagListDto(String portfolioTagList, String plannerTagList, String dressTagList,
		String studioTypeTagList, String studioFocusTagList, String makeupTagList) {
		this.portfolioTagList = portfolioTagList;
		this.plannerTagList = plannerTagList;
		this.dressTagList = dressTagList;
		this.studioTypeTagList = studioTypeTagList;
		this.studioFocusTagList = studioFocusTagList;
		this.makeupTagList = makeupTagList;
	}
}
