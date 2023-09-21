package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.CustomerDuplicateRegistrationException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerDuplicateRegistrationException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;

@Transactional
@Service
@RequiredArgsConstructor
public class UsersService {

	private final UsersRepository usersRepository;

	@Transactional
	public UserRegisterStatusEnum getUserRegisterStatus(Users users) {
		Users pUsers = findUserByEmail(users.getEmail());
		return pUsers.getRegisterStatus();
	}



	/*================== Repository 접근 ==================*/
	@Transactional(readOnly = true)
	public Users findUserById(Long id) {
		return usersRepository.findById(id)
			.orElseThrow(UserNotFoundException::new);
	}

	@Transactional(readOnly = true)
	public Users findUserByNickname(String nickname) {
		return usersRepository.findByNickname(nickname)
			.orElseThrow(UserNotFoundException::new);
	}

	@Transactional(readOnly = true)
	public Users findUserByEmail(String email) {
		return usersRepository.findByEmail(email)
			.orElseThrow(UserNotFoundException::new);
	}

	@Transactional(readOnly = true)
	public Users findUserByPlanner(Planners planners) {
		return usersRepository.findByPlanners(planners)
			.orElseThrow(UserNotFoundException::new);
	}

	@Transactional(readOnly = true)
	public List<Users> findAllPlannerUser() {
		return usersRepository.findAllByRegisterStatus(UserRegisterStatusEnum.PLANNER);
	}

	@Transactional(readOnly = true)
	public Users findUserByCustomer(Customers customers) {
		return usersRepository.findByCustomers(customers)
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