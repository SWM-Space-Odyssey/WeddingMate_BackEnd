package swmaestro.spaceodyssey.weddingmate.domain.file.controller;

import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FilesService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/file")
public class FilesController {

	private final FilesService fileService;

	@GetMapping()
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getImagePage(@PageableDefault(size = 18) Pageable pageable) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(fileService.getImage(pageable))
			.build();
	}
}