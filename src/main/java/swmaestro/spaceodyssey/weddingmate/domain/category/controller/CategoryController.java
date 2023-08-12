package swmaestro.spaceodyssey.weddingmate.domain.category.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.service.CategoryService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/category")
public class CategoryController {

	private final CategoryService categoryService;

	@GetMapping("/all")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getAllCategory() {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(categoryService.findAllCategory())
			.build();
	}
}
