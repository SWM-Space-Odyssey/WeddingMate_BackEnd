package swmaestro.spaceodyssey.weddingmate.domain.profile.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.ProfileService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class UserProfileController { // profileController가 spring 기본 config 클래스로 정의되어 있음

	private final ProfileService profileService;

	@GetMapping("/planner")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPlannerProfile(@AuthUsers Users users) {
		PlannerProfileResDto resDto = profileService.getPlannerProfile(users);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resDto)
			.build();
	}

	@PutMapping("/planner")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updatePlannerProfile(@RequestBody PlannerProfileUpdateReqDto reqDto) {
		PlannerProfileUpdateResDto resDto = profileService.updatePlannerProfile(reqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resDto)
			.build();
	}
}