package swmaestro.spaceodyssey.weddingmate.domain.company.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.company.repository.CompaniesRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.company.CompanyNotFoundException;

@Service
@Transactional
@RequiredArgsConstructor
public class CompaniesService {
	private final CompaniesRepository companiesRepository;
	/*================== Repository 접근 ==================*/
	@Transactional(readOnly = true)
	public Companies findCompanyById(Long id) {
		return companiesRepository.findById(id)
			.orElseThrow(CompanyNotFoundException::new);
	}

}
