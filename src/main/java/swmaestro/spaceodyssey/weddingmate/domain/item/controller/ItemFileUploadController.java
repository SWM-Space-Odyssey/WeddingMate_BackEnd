package swmaestro.spaceodyssey.weddingmate.domain.item.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemFileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.service.ItemFileUploadService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/portfolio/item/file")
public class ItemFileUploadController {

	private final ItemFileUploadService itemFileUploadService;

	@PostMapping()
	@ResponseStatus(HttpStatus.CREATED)
	public ApiResponse<Object> postItem(@RequestPart ItemFileReqDto itemFileReqDto,
		@NotNull @RequestPart("file") MultipartFile multipartFile) {

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(itemFileUploadService.uploadItemFile(itemFileReqDto, multipartFile))
			.build();
	}
}
