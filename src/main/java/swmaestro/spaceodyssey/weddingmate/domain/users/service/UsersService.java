package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.CustomerMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannerMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.CustomerRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannerRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.CustomerDuplicateRegistrationException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerDuplicateRegistrationException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;

@Transactional
@Service
@RequiredArgsConstructor
public class UsersService {

	private final UsersRepository usersRepository;
	private final PlannerRepository plannerRepository;
	private final CustomerRepository customerRepository;

	private final PlannerMapper plannerMapper;
	private final CustomerMapper customerMapper;

	@Transactional
	public UserRegisterStatusEnum getUserRegisterStatus(Users users) {
		Users pUsers = findUserByEmail(users.getEmail());
		return pUsers.getRegisterStatus();
	}

	@Transactional
	public void signupPlanner(Users users, PlannerSignupReqDto reqDto) {
		Users pUsers = findUserByEmail(users.getEmail());
		checkUserIsRegistered(users);

		pUsers.updateNickname(reqDto.getNickname());
		pUsers.updateRegisterStatus(UserRegisterStatusEnum.PLANNER);

		Planner planner = createPlanner(pUsers, reqDto);
		PlannerProfile plannerProfile = createPlannerProfile(planner); // JPA에 의해 자동으로 planner에 반영

		plannerRepository.save(planner);
	}

	@Transactional
	public void signupCustomer(Users users, CustomerSignupReqDto reqDto) {
		Users pUsers = findUserByEmail(users.getEmail());
		checkUserIsRegistered(users);

		pUsers.updateNickname(reqDto.getNickname());
		pUsers.updateRegisterStatus(UserRegisterStatusEnum.CUSTOMER);

		Customer customer = createCustomer(users, reqDto);
		customerRepository.save(customer);
	}

	@Transactional
	public Planner createPlanner(Users users, PlannerSignupReqDto reqDto) {
		Planner planner = plannerMapper.toEntity(reqDto);
		planner.setUsers(users);
		return planner;
	}

	@Transactional
	public PlannerProfile createPlannerProfile(Planner planner) {
		PlannerProfile plannerProfile = PlannerProfile.builder()
			.bio("반갑습니다.")
			.build();
		plannerProfile.setPlanner(planner);
		return plannerProfile;
	}

	@Transactional
	public Customer createCustomer(Users users, CustomerSignupReqDto reqDto) {
		Customer customer = customerMapper.toEntity(reqDto);
		customer.setUsers(users);
		return customer;
	}

	/*================== Repository 접근 ==================*/
	@Transactional(readOnly = true)
	public Users findUserByEmail(String email) {
		return usersRepository.findByEmail(email)
			.orElseThrow(UserNotFoundException::new);
	}

	/*================== 예외 처리 ==================*/
	@Transactional(readOnly = true)
	public void checkUserIsRegistered(Users users) {
		UserRegisterStatusEnum status = users.getRegisterStatus();
		if (status == UserRegisterStatusEnum.PLANNER) {
			throw new PlannerDuplicateRegistrationException();
		} else if (status == UserRegisterStatusEnum.CUSTOMER) {
			throw new CustomerDuplicateRegistrationException();
		}
	}
}