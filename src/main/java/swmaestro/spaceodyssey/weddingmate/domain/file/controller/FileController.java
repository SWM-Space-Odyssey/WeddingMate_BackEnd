package swmaestro.spaceodyssey.weddingmate.domain.file.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FeedResDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/file")
public class FileController {

	private final FileService fileService;

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