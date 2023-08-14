package swmaestro.spaceodyssey.weddingmate.domain.profile.controller;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.ProfileService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequestMapping("/api/v1/planner")
@RequiredArgsConstructor
public class PlannerProfileController { // profileController가 spring 기본 config 클래스로 정의되어 있음

	private final ProfileService profileService;

	@GetMapping()
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getAllPlannerProfileList() {
		List<PlannerProfileResDto> resDtoList = profileService.getPlannerProfileList();
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resDtoList)
			.build();
	}

	@GetMapping("/profile")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPlannerProfileByProfileId(@RequestBody PlannerProfileReqDto plannerProfileReqDto) {
		PlannerProfileResDto plannerProfileResDto = profileService.getPlannerProfileByProfileId(plannerProfileReqDto);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(plannerProfileResDto)
			.build();
	}

	@PutMapping("/profile")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updatePlannerProfile(@AuthUsers Users users, @RequestBody PlannerProfileUpdateReqDto reqDto) {
		PlannerProfileUpdateResDto resDto = profileService.updatePlannerProfile(users, reqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resDto)
			.build();
	}
}