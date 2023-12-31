package swmaestro.spaceodyssey.weddingmate.domain.item.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemFileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.service.ItemsFilesUploadService;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Image File API", description = "아이템 이미지 파일 관련 API")
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/portfolio/item/file")
public class ItemsFilesUploadController {

	private final ItemsFilesUploadService itemsFilesUploadService;

	@Operation(summary = "아이템 내의 이미지 파일 업로드")
	@PostMapping()
	@ResponseStatus(HttpStatus.CREATED)
	public ApiResponse<Object> postItem(@RequestPart ItemFileReqDto itemFileReqDto,
		@NotNull @RequestPart("file") MultipartFile multipartFile) {

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(itemsFilesUploadService.uploadItemFile(itemFileReqDto, multipartFile))
			.build();
	}
}
