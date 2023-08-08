package swmaestro.spaceodyssey.weddingmate.domain.tag.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.tag.service.TagService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/tag")
public class TagController {
	private final TagService tagService;

	@GetMapping("/all")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getAllTag(@RequestParam String category) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(tagService.findAllTag(category))
			.build();
	}
}
