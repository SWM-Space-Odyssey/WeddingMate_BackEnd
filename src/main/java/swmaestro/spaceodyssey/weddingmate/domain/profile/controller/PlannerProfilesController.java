package swmaestro.spaceodyssey.weddingmate.domain.profile.controller;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.PlannerProfilesService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequestMapping("/api/v1/planner")
@RequiredArgsConstructor
public class PlannerProfilesController { // profileController가 spring 기본 config 클래스로 정의되어 있음

	private final PlannerProfilesService plannerProfilesService;

	@GetMapping()
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getAllPlannerProfileList(@AuthUsers Users users) {
		List<PlannerProfileResDto> resDtoList = plannerProfilesService.getPlannerProfileList(users);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resDtoList)
			.build();
	}

	@GetMapping("/{userId}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPlannerProfileByProfileId(@AuthUsers Users users, @PathVariable Long userId) {
		PlannerProfileResDto plannerProfileResDto = plannerProfilesService.getPlannerProfileByUserId(users, userId);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(plannerProfileResDto)
			.build();
	}

	@GetMapping("/{userId}/portfolio")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPlannerPortfolioByProfileId(@AuthUsers Users users, @PathVariable Long userId) {
		List<PortfolioListResDto> portfolioListResDto = plannerProfilesService.getPlannerPortfolioByProfileId(users, userId);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(portfolioListResDto)
			.build();
	}
}