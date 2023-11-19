package swmaestro.spaceodyssey.weddingmate.domain.company.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.company.repository.CompaniesRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.company.CompanyNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class CompaniesRepositoryService {

	private final CompaniesRepository companiesRepository;

	public Companies findCompanyById(Long id) {
		return companiesRepository.findById(id)
			.orElseThrow(CompanyNotFoundException::new);
	}

	public List<Companies> searchCompaniesByFullText(String keyword) {
		return companiesRepository.searchByFullText(keyword);
	}
}
