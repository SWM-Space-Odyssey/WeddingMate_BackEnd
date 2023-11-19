package swmaestro.spaceodyssey.weddingmate.domain.company.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class CompanySearchResDto {
	private String name;
	private String repImg;
	private String category;
	private Boolean isLiked;
	private Long companyId;

	@Builder
	public CompanySearchResDto(String name, String repImg, String category, Boolean isLiked, Long companyId) {
		this.name = name;
		this.repImg = repImg;
		this.category = category;
		this.isLiked = isLiked;
		this.companyId = companyId;
	}
}
