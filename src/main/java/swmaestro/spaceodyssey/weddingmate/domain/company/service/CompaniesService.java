package swmaestro.spaceodyssey.weddingmate.domain.company.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.dto.CompanyItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.company.dto.CompanyResDto;
import swmaestro.spaceodyssey.weddingmate.domain.company.dto.CompanySearchResDto;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.company.mapper.CompanyMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Service
@Transactional
@RequiredArgsConstructor
public class CompaniesService {
	private final CompaniesRepositoryService companiesRepositoryService;
	private final CompanyMapper companyMapper;

	public CompanyResDto findById(Long id) {
		Companies company = companiesRepositoryService.findCompanyById(id);

		List<String> imageList = companyMapper.entityToImageDtoByLimit(company);

		boolean isMore = imageList.size() == 10;

		return CompanyResDto.builder()
			.name(company.getName())
			.address(company.getAddress())
			.imageList(imageList)
			.itemList(company.getItems().stream().map(companyMapper::entityToItemDto).limit(2).toList())
			.isMoreThanNineImages(isMore)
			.build();
	}

	public List<CompanyItemResDto> findItemById(Long id) {
		Companies company = companiesRepositoryService.findCompanyById(id);

		return company.getItems().stream().map(companyMapper::entityToItemDto).toList();
	}

	public List<String> findImagesById(Long id) {
		Companies company = companiesRepositoryService.findCompanyById(id);

		return companyMapper.entityToImageDto(company);
	}

	public List<CompanySearchResDto> searchCompanyByFullText(Users user, String keyword) {
		List<Companies> companies = companiesRepositoryService.searchCompaniesByFullText(keyword);

		return companies.stream()
			.map(company -> companyMapper.entityToSearchDto(company, user))
			.toList();
	}
}
