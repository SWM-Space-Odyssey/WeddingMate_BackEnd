package swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;

import java.util.List;

@Transactional
@Service
@RequiredArgsConstructor
public class UsersRepositoryService {

	private final UsersRepository usersRepository;

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

	@Transactional(readOnly = true)
	public List<Users> findAllSuspendedUsers() {
		return usersRepository.findAllSuspendedUsers();
	}
}
