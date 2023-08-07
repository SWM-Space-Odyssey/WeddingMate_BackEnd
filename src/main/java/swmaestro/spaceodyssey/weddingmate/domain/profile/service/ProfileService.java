package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileService;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;
import swmaestro.spaceodyssey.weddingmate.domain.profile.mapper.ProfileMapper;
import swmaestro.spaceodyssey.weddingmate.domain.profile.repository.PlannerProfileRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannerRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.PlannerProfileNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class ProfileService {

	private final PlannerProfileRepository plannerProfileRepository;
	private final PlannerRepository plannerRepository;
	private final ProfileMapper profileMapper;
	private final FileService fileService;

	public PlannerProfileResDto getPlannerProfile(Users users) {
		Planner planner = findPlannerByUsers(users);
		File profileImage = fileService.findById(users.getProfileImage().getFileId());

		return profileMapper.toPlannerProfileResDto(profileImage.getUrl(), planner.getPlannerProfile());
	}

	@Transactional
	public PlannerProfileUpdateResDto updatePlannerProfile(PlannerProfileUpdateReqDto reqDto) {
		PlannerProfile plannerProfile = findPlannerProfileById(reqDto.getPlannerProfileId());

		if (reqDto.getBio() != null) {
			plannerProfile.updateBio(reqDto.getBio());
		}
		if (reqDto.getSns() != null) {
			plannerProfile.updateSns(reqDto.getSns());
		}

		return profileMapper.toPlannerProfileUpdateResDto(plannerProfile);
	}

	@Transactional
	public Planner findPlannerByUsers(Users users) {
		return plannerRepository.findByUsers(users)
			.orElseThrow(PlannerNotFoundException::new);
	}

	@Transactional
	public PlannerProfile findPlannerProfileById(Long plannerProfileId) {
		return plannerProfileRepository.findById(plannerProfileId)
			.orElseThrow(PlannerProfileNotFoundException::new);
	}
}
