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
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.PlannerProfileService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequestMapping("/api/v1/planner")
@RequiredArgsConstructor
public class PlannerProfileController { // profileController가 spring 기본 config 클래스로 정의되어 있음

	private final PlannerProfileService plannerProfileService;

	@GetMapping()
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getAllPlannerProfileList() {
		List<PlannerProfileResDto> resDtoList = plannerProfileService.getPlannerProfileList();
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resDtoList)
			.build();
	}

	@GetMapping("/{plannerProfileId}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPlannerProfileByProfileId(@AuthUsers Users users, @PathVariable Long plannerProfileId) {
		PlannerProfileResDto plannerProfileResDto = plannerProfileService.getPlannerProfileByProfileId(users, plannerProfileId);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(plannerProfileResDto)
			.build();
	}

	@GetMapping("/{plannerProfileId}/portfolio")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPlannerPortfolioByProfileId(@AuthUsers Users users, @PathVariable Long plannerProfileId) {
		List<PortfolioListResDto> portfolioListResDto = plannerProfileService.getPlannerPortfolioByProfileId(users, plannerProfileId);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(portfolioListResDto)
			.build();
	}
}