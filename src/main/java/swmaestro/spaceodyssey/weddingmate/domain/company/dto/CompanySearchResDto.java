package swmaestro.spaceodyssey.weddingmate.domain.company.dto;

import lombok.Builder;
import lombok.Getter;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;

@Getter
public class CompanySearchResDto {
	private String name;
	private String category;
	private boolean isLiked;
	private Long companyId;

	@Builder
	public CompanySearchResDto(String name, String category, boolean isLiked, Long companyId) {
		this.name = name;
		this.category = category;
		this.isLiked = isLiked;
		this.companyId = companyId;
	}

	public static CompanySearchResDto of(Companies company, boolean isLiked) {
		return CompanySearchResDto.builder()
			.name(company.getName())
			.category(company.getCategory())
			.companyId(company.getCompanyId())
			.isLiked(isLiked)
			.build();
	}
}
