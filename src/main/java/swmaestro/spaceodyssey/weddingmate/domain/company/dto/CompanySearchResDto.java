package swmaestro.spaceodyssey.weddingmate.domain.company.dto;

import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;

public record CompanySearchResDto(String name, String category, boolean isLiked, Long companyId) {

	private static CompanySearchResDto of(final Companies company, final boolean isLiked) {
		return new CompanySearchResDto(
			company.getName(),
			company.getCategory(),
			isLiked,
			company.getCompanyId()
		);
	}

	public static CompanySearchResDto fromLiked(final Companies company, final boolean isLiked) {
		return of(company, isLiked);
	}
}
