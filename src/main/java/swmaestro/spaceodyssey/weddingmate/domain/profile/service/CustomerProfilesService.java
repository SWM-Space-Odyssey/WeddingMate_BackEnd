package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper.PortfoliosMapper;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.CustomerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.CustomersMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.CustomersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;

@Service
@RequiredArgsConstructor
@Transactional
public class CustomerProfilesService {

	private final UsersRepositoryService usersRepositoryService;
	private final CustomersRepositoryService customersRepositoryService;

	private final CustomersMapper customersMapper;
	private final PortfoliosMapper portfoliosMapper;

	public CustomerProfileResDto getCustomerProfileById(Long userId) {
		Users users = usersRepositoryService.findUserById(userId);
		Customers customers = customersRepositoryService.findCustomerByUser(users);

		return CustomerProfileResDto.builder()
			.userId(userId)
			.nickname(users.getNickname())
			.profileImageUrl(users.getProfileImage().getUrl())
			.customerTagListDto(customersMapper.toCustomerTagListDto(customers))
			.customerInfoDto(customersMapper.toCustomerInfoDto(customers))
			.build();
	}

	public List<PortfolioListResDto> getPortfolioByUserId(Users cUsers, Long userId) {
		Users users = usersRepositoryService.findUserById(userId);

		return users.getPortfoliosList().stream()
			//삭제된 portfolio 제외
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(portfolio -> portfoliosMapper.entityToDto(cUsers, portfolio))
			.toList();
	}
}
