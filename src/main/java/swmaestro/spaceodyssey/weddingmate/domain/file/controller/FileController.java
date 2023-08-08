package swmaestro.spaceodyssey.weddingmate.domain.file.controller;

import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.ItemFileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileService;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/file")
public class FileController {

	private final FileUploadService fileUploadService;
	private final FileService fileService;

	@PostMapping("/items")
	@ResponseStatus(HttpStatus.CREATED)
	public ApiResponse<Object> postItem(@RequestPart ItemFileReqDto itemFileReqDto,
											@NotNull @RequestPart("file") MultipartFile multipartFile) {

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(fileUploadService.uploadItemFile(itemFileReqDto, multipartFile))
			.build();
	}

	@PostMapping("/profile")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updateProfile(@AuthUsers Users users,
												@NotNull @RequestPart("file") MultipartFile multipartFile) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(fileUploadService.updateProfileFile(users, multipartFile))
			.build();
	}

	@DeleteMapping("/profile")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> deleteProfile(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(fileUploadService.deleteProfileFile(users))
			.build();
  }

	@GetMapping()
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getImagePage(@PageableDefault(size = 18) Pageable pageable) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(fileService.getImage(pageable))
			.build();
	}
}
