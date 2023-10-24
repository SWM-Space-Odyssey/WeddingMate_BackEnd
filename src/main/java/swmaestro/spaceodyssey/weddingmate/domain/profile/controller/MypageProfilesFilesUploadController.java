package swmaestro.spaceodyssey.weddingmate.domain.profile.controller;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
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
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.ProfilesFilesUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Profile Image File API", description = "프로필 이미지 파일 관련 API")
@RestController
@RequestMapping("/api/v1/profile/file")
@RequiredArgsConstructor
public class MypageProfilesFilesUploadController {

	private final ProfilesFilesUploadService profilesFilesUploadService;

	@Operation(summary = "프로필 이미지 파일 수정")
	@PostMapping()
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updateProfile(@AuthUsers Users users,
		@NotNull @RequestPart("file") MultipartFile multipartFile) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(profilesFilesUploadService.updateProfileFile(users, multipartFile))
			.build();
	}

	@Operation(summary = "프로필 이미지 파일 삭제")
	@DeleteMapping()
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> deleteProfile(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(profilesFilesUploadService.deleteProfileFile(users))
			.build();
	}
}
