package swmaestro.spaceodyssey.weddingmate.domain.profile.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.ProfileService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class UserProfileController { // profileController가 spring 기본 config 클래스로 정의되어 있음

	private final ProfileService profileService;

	@GetMapping("/planner")
	public ResponseEntity<PlannerProfileResDto> getPlannerProfile(@AuthUsers Users users) {
		PlannerProfileResDto resDto = profileService.getPlannerProfile(users);
		return new ResponseEntity<>(resDto, HttpStatus.OK);
	}

	@PutMapping("/planner")
	public ResponseEntity<PlannerProfileResDto> updatePlannerProfile(@RequestBody PlannerProfileUpdateDto reqDto) {
		PlannerProfileResDto resDto = profileService.updatePlannerProfile(reqDto);
		return new ResponseEntity<>(resDto, HttpStatus.OK);
	}
}