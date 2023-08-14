package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.CustomerMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.CustomerRepository;

@Transactional
@Service
@RequiredArgsConstructor
public class CustomerService {

	private final UsersService usersService;
	private final CustomerMapper customerMapper;
	private final CustomerRepository customerRepository;

	@Transactional
	public void signupCustomer(Users users, CustomerSignupReqDto reqDto) {
		Users pUsers = usersService.findUserByEmail(users.getEmail());
		usersService.checkUserIsRegistered(users);

		pUsers.updateNickname(reqDto.getNickname());
		pUsers.updateRegisterStatus(UserRegisterStatusEnum.CUSTOMER);

		Customer customer = createCustomer(users, reqDto);
		customerRepository.save(customer);
	}

	@Transactional
	public Customer createCustomer(Users users, CustomerSignupReqDto reqDto) {
		Customer customer = customerMapper.toEntity(reqDto);
		customer.setUsers(users);
		return customer;
	}

	/*================== Repository 접근 ==================*/

}
