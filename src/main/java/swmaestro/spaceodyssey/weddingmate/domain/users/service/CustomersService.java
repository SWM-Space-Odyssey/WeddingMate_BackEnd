package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.CustomersMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.CustomersRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;

@Transactional
@Service
@RequiredArgsConstructor
public class CustomersService {

	private final UsersService usersService;
	private final CustomersMapper customersMapper;
	private final CustomersRepository customersRepository;
	private final UsersRepositoryService usersRepositoryService;

	@Transactional
	public void signupCustomer(Users users, CustomerSignupReqDto reqDto) {
		Users pUsers = usersRepositoryService.findUserByEmail(users.getEmail());
		usersService.checkUserIsRegistered(users);

		pUsers.updateNickname(reqDto.getNickname());
		pUsers.updateRegisterStatus(UserRegisterStatusEnum.CUSTOMER);

		Customers customers = createCustomer(pUsers, reqDto);
		customersRepository.save(customers);
	}

	@Transactional
	public Customers createCustomer(Users users, CustomerSignupReqDto reqDto) {
		Customers customers = customersMapper.toEntity(reqDto);
		customers.setUsers(users);
		return customers;
	}

}
