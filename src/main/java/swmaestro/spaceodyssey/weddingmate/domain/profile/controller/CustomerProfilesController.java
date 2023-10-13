package swmaestro.spaceodyssey.weddingmate.domain.profile.controller;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.CustomerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.CustomerProfilesService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Profile Info API", description = "프로필 조회 관련 API")
@RestController
@RequestMapping("/api/v1/customer")
@RequiredArgsConstructor
public class CustomerProfilesController {
	private final CustomerProfilesService customerProfilesService;

	@Operation(summary = "userId로 해당하는 예비부부 프로필 조회")
	@GetMapping("/{userId}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getCustomerProfileById(@AuthUsers Users users, @PathVariable Long userId) {
		CustomerProfileResDto customerProfileResDto = customerProfilesService.getCustomerProfileById(userId);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(customerProfileResDto)
			.build();
	}

	@Hidden
	@GetMapping("/{userId}/portfolio")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPlannerPortfolioByProfileId(@AuthUsers Users users, @PathVariable Long userId) {
		List<PortfolioListResDto> portfolioListResDto = customerProfilesService.getPortfolioByUserId(users, userId);

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(portfolioListResDto)
			.build();
	}
}
