package swmaestro.spaceodyssey.weddingmate.domain.users.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserAccountStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.CustomersService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.PlannersService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.UsersService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Signup API", description = "회원가입 API")
@RestController
@RequestMapping("/api/v1/signup")
@RequiredArgsConstructor
public class SignupController {

	private final UsersService usersService;
	private final PlannersService plannerService;
	private final CustomersService customersService;

	@Operation(summary = "플래너 회원가입")
	@PostMapping("/planner")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ApiResponse<Object> plannerSignup(@AuthUsers Users users, @RequestBody PlannerSignupReqDto reqDto) {
		plannerService.signupPlanner(users, reqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(reqDto.getNickname() + PLANNER_SIGNUP_SUCCESS)
			.build();
	}

	@Operation(summary = "예비부부 회원가입")
	@PostMapping("/customer")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ApiResponse<Object> customerSignup(@AuthUsers Users users, @RequestBody CustomerSignupReqDto reqDto) {
		customersService.signupCustomer(users, reqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(reqDto.getNickname() + CUSTOMER_SIGNUP_SUCCESS)
			.build();
	}

	@Operation(summary = "유저의 가입 상태 확인(UNREGISTERED, CUSTOMER, PLANNER")
	@GetMapping()
	@ResponseStatus(value = HttpStatus.OK)
	public ApiResponse<Object> checkUserRegisterStatus(@AuthUsers Users users) {
		Object responseData;

		UserAccountStatusEnum usersAccountStatus = usersService.checkUsersAccountStatus(users);
		if (usersAccountStatus == UserAccountStatusEnum.NORMAL) {
			responseData = usersService.getUserRegisterStatus(users);
		} else {
			responseData = getAccountStatusMessage(usersAccountStatus);
		}

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(responseData)
			.build();
	}

	private String getAccountStatusMessage(UserAccountStatusEnum accountStatus) {
		return switch (accountStatus) {
			case WITHDRAW -> ACCOUNT_WITHDRAW_MESSAGE;
			case SUSPENDED -> ACCOUNT_SUSPENDED_MESSAGE;
			case BANNED -> ACCOUNT_BANNED_MESSAGE;
			default -> "";
		};
	}
}