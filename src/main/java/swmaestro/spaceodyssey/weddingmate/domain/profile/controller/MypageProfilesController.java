package swmaestro.spaceodyssey.weddingmate.domain.profile.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.CustomerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.CustomerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.ProfilesService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Profile Info API", description = "자신의 프로필 관련 API")
@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class MypageProfilesController {

	private final ProfilesService profilesService;

	@Hidden
	@GetMapping("/planner")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPlannerProfile(@AuthUsers Users users) {
		PlannerProfileResDto plannerProfileResDto = profilesService.getPlannerProfile(users);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(plannerProfileResDto)
			.build();
	}

	@Hidden
	@PutMapping("/planner")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updatePlannerProfile(@AuthUsers Users users,
		@RequestBody PlannerProfileUpdateReqDto reqDto) {
		PlannerProfileUpdateResDto resDto = profilesService.updatePlannerProfile(users, reqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resDto)
			.build();
	}

	@Operation(summary = "자신의 프로필 정보 조회(예비부부)")
	@GetMapping("/customer")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getCustomerProfile(@AuthUsers Users users) {
		CustomerProfileResDto customerProfileResDto = profilesService.getCustomerProfile(users);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(customerProfileResDto)
			.build();
	}

	@Operation(summary = "자신의 프로필 정보 수정(예비부부)")
	@PutMapping("/customer")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updateCustomerProfile(@AuthUsers Users users,
		@RequestBody CustomerProfileUpdateReqDto reqDto) {
		profilesService.updateCustomerProfile(users, reqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(CUSTOMER_PROFILE_UPDATE_SUCCESS)
			.build();
	}
}
