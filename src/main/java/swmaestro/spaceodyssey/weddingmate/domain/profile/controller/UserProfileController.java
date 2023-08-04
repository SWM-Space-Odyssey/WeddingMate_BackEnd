package swmaestro.spaceodyssey.weddingmate.domain.profile.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.ProfileService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class UserProfileController { // profileController가 spring 기본 config 클래스로 정의되어 있음

	private final ProfileService profileService;
	private final FileUploadService fileUploadService;

	@PostMapping("/image")
	public ResponseEntity<String> updateProfileImage(@AuthUsers Users users,
													@RequestPart FileReqDto fileReqDto,
													@NotNull @RequestPart("file") MultipartFile multipartFile) {
		String updatedImageId = profileService.updateProfileImage(users, fileReqDto, multipartFile);
		return ResponseEntity.ok().body(uploadedImageId);
	}

	@DeleteMapping("/image")
	public ResponseEntity<String> deleteProfileImage() {
		String basicImageId = profileService.deleteProfileImage();
		return ResponseEntity.ok().body(basicImageId);
	}

	@GetMapping("/planner")
	public ResponseEntity<PlannerProfileResDto> getPlannerProfile(@AuthUsers Users users) {
		PlannerProfileResDto resDto = profileService.getPlannerProfile(users);
		return new ResponseEntity<>(resDto, HttpStatus.OK);
	}

	@PutMapping("/planner")
	public ResponseEntity<PlannerProfileUpdateResDto> updatePlannerProfileInfo(@RequestBody PlannerProfileUpdateReqDto reqDto) {
		PlannerProfileUpdateResDto resDto = profileService.updatePlannerProfile(reqDto);
		return new ResponseEntity<>(resDto, HttpStatus.OK);
	}
}
