package swmaestro.spaceodyssey.weddingmate.domain.company.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.service.CompaniesService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "COMPANY API", description = "업체 관련 API")
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/company")
public class CompanyController {
	private final CompaniesService companyService;

	@Operation(summary = "id값으로 해당하는 업체 호출")
	@GetMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getCompanyId(@AuthUsers Users users, @PathVariable("id") Long id) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(companyService.findById(id))
			.build();
	}

	@Operation(summary = "업체 id값으로 이미지 리스트 호출")
	@GetMapping("/{id}/images")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getImagesById(@AuthUsers Users users, @PathVariable("id") Long id) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(companyService.findImagesById(id))
			.build();
	}

	@Operation(summary = "업체 id값으로 아이템 리스트 호출")
	@GetMapping("/{id}/items")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getItemsById(@AuthUsers Users users, @PathVariable("id") Long id) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(companyService.findItemById(id))
			.build();
	}
}
