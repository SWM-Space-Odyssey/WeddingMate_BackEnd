package swmaestro.spaceodyssey.weddingmate.domain.profile.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class CustomerInfoDto {
	private Boolean weddingDateConfirmed;
	private String weddingDate;
	private String budget;
	private String regionList;

	@Builder
	public CustomerInfoDto(Boolean weddingDateConfirmed, String weddingDate, String budget, String regionList) {
		this.weddingDateConfirmed = weddingDateConfirmed;
		this.weddingDate = weddingDate;
		this.budget = budget;
		this.regionList = regionList;
	}
}
