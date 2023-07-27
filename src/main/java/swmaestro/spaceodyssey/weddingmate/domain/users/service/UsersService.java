package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.CustomerMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannerMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.CustomerRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannerRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;

@Service
@RequiredArgsConstructor
public class UsersService {

	private final UsersRepository usersRepository;
	private final PlannerRepository plannerRepository;
	private final CustomerRepository customerRepository;

	private final PlannerMapper plannerMapper;
	private final CustomerMapper customerMapper;

	@Transactional
	public void signupPlanner(Users users, PlannerSignupReqDto reqDto) {
		Users pUsers = findUserByEmail(users.getEmail());
		pUsers.updateNickname(reqDto.getNickname());

		Planner planner = plannerMapper.toEntity(reqDto);
		planner.setUsers(pUsers);
		plannerRepository.save(planner);
	}

	@Transactional
	public void signupCustomer(Users users, CustomerSignupReqDto reqDto) {
		Users pUsers = findUserByEmail(users.getEmail());
		pUsers.updateNickname(reqDto.getNickname());

		Customer customer = customerMapper.toEntity(reqDto);
		customer.setUsers(pUsers);
		customerRepository.save(customer);
	}

	@Transactional
	public Users findUserByEmail(String email) {
		return usersRepository.findByEmail(email)
			.orElseThrow(UserNotFoundException::new);
	}
}