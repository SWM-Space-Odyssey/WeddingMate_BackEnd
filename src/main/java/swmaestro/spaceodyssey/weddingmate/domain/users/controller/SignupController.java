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
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.UsersService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequestMapping("/api/v1/signup")
@RequiredArgsConstructor
public class SignupController {

	private final UsersService usersService;

	@PostMapping("/planner")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ApiResponse<Object> plannerSignup(@AuthUsers Users users, @RequestBody PlannerSignupReqDto reqDto) {
		usersService.signupPlanner(users, reqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(reqDto.getNickname() + PLANNER_SIGNUP_SUCCESS)
			.build();
	}

	@PostMapping("/customer")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ApiResponse<Object> customerSignup(@AuthUsers Users users, @RequestBody CustomerSignupReqDto reqDto) {
		usersService.signupCustomer(users, reqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(reqDto.getNickname() + CUSTOMER_SIGNUP_SUCCESS)
			.build();
	}

	@GetMapping()
	@ResponseStatus(value = HttpStatus.OK)
	public ApiResponse<Object> checkUserRegisterStatus(@AuthUsers Users users) {
		UserRegisterStatusEnum userRegisterStatusEnum = usersService.getUserRegisterStatus(users);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(userRegisterStatusEnum)
			.build();
	}
}
