package swmaestro.spaceodyssey.weddingmate.domain.users.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.UsersService;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;

@RestController
@RequestMapping("/api/v1/signup")
@RequiredArgsConstructor
public class SignupController {

	private final UsersService usersService;
	private final UsersRepository usersRepository;

	@PostMapping("/planner")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<String> plannerSignup(@AuthUsers Users users, @RequestBody PlannerSignupReqDto reqDto) {
		usersService.signupPlanner(users, reqDto);
		return ResponseEntity.ok().body(reqDto.getNickname() + PLANNER_SIGNUP_SUCCESS);
	}

	@PostMapping("/customer")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<String> customerSignup(@AuthUsers Users users, @RequestBody CustomerSignupReqDto reqDto) {
		usersService.signupCustomer(users, reqDto);
		return ResponseEntity.ok().body(reqDto.getNickname() + CUSTOMER_SIGNUP_SUCCESS);
	}

	@GetMapping("/")
	public ResponseEntity<String> test(@AuthUsers Users users){
		Users pUsers = usersRepository.findByEmail(users.getEmail())
			.orElseThrow(UserNotFoundException::new);
		pUsers.updateNickname("테스트닉네임");

		Users nUsers = usersRepository.findByNickname(pUsers.getNickname())
			.orElseThrow(UserNotFoundException::new);
		return ResponseEntity.ok().body(nUsers.getEmail() + ' ' + nUsers.getNickname());
	}
}
