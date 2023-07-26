package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import jakarta.persistence.EntityManager;
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
		Users pUsers = findUserByEmail(users, reqDto.getNickname());
		pUsers.updateNickname(reqDto.getNickname());

		Planner planner = plannerMapper.toEntity(pUsers, reqDto);
		plannerRepository.save(planner);
	}

	@Transactional
	public void signupCustomer(Users users, CustomerSignupReqDto reqDto) {
		System.out.println("서비스 진입");
		Users pUsers = findUserByEmail(users, reqDto.getNickname());
		pUsers.updateNickname(reqDto.getNickname());

		System.out.println("유저 닉네임 업데이트 완");
		Customer customer = customerMapper.toEntity(pUsers, reqDto);
		customerRepository.save(customer);
	}

	@Transactional
	public Users findUserByEmail(Users users, String nickname) {
		Users pUsers = usersRepository.findByEmail(users.getEmail())
			.orElseThrow(UserNotFoundException::new);
		if (isEntityManaged(pUsers)){
			return pUsers;
		}
		else
			return null;
	}

	@Autowired
	private EntityManager entityManager;

	public boolean isEntityManaged(Users user) {
		return entityManager.contains(user);
	}
}
