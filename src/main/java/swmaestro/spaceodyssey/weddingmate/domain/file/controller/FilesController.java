package swmaestro.spaceodyssey.weddingmate.domain.file.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FeedResDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FilesService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "File API", description = "이미지 파일 관련 API")
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/file")
public class FilesController {

	private final FilesService fileService;

	@Operation(summary = "커서 기반 페이지네이션으로 이미지 목록 제공")
	@Parameter(name = "cursor", description = "커서 시작 값(required false)")
	@Parameter(name = "pageSize", description = "리턴할 페이지의 개수(default 18개)")
	@GetMapping()
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getImagePage(@RequestParam(required = false) Long cursor,
		@RequestParam(defaultValue = "18") int pageSize) {
		FeedResDto feedResDto;
		if (cursor != null) {
			feedResDto = fileService.getImagesAfterCursor(cursor, pageSize);
		} else {
			feedResDto = fileService.getFirstPageImages(pageSize);
		}
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(feedResDto)
			.build();
	}
}