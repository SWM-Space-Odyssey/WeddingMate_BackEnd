package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;
import swmaestro.spaceodyssey.weddingmate.domain.profile.mapper.ProfileMapper;
import swmaestro.spaceodyssey.weddingmate.domain.profile.repository.PlannerProfileRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannerMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.PlannerService;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.PlannerProfileNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.ProfileModificationNotAllowedException;

@Service
@RequiredArgsConstructor
@Transactional
public class ProfileService {

	private final PlannerProfileRepository plannerProfileRepository;

	private final PlannerMapper plannerMapper;
	private final ProfileMapper profileMapper;

	private final PlannerService plannerService;

	public PlannerProfileResDto getPlannerProfile(Users users) {
		Planner planner = plannerService.findPlannerByUser(users);
		PlannerProfile plannerProfile = planner.getPlannerProfile(); // EAGER로 인해 즉시 로딩 가능

		return PlannerProfileResDto.builder()
			.nickname(users.getNickname())
			.profileImageUrl(users.getProfileImage().getUrl())
			.plannerInfo(plannerMapper.toPlannerInfoDto(planner))
			.plannerProfileInfo(profileMapper.toPlannerProfileInfoResDto(plannerProfile))
			.build();
	}

	@Transactional
	public PlannerProfileUpdateResDto updatePlannerProfile(Users users, PlannerProfileUpdateReqDto reqDto) {
		Planner planner = plannerService.findPlannerByUser(users);
		PlannerProfile plannerProfile = planner.getPlannerProfile(); // EAGER로 인해 즉시 로딩 가능

		if (reqDto.getBio() != null) {
			plannerProfile.updateBio(reqDto.getBio());
		}
		if (reqDto.getSns() != null) {
			plannerProfile.updateSns(reqDto.getSns());
		}
		planner.updatePlannerTagList(reqDto.getTagList());

		return profileMapper.toPlannerProfileUpdateResDto(plannerProfile, planner.getPlannerTagList());
	}

	/*================== Repository 접근 ==================*/
	@Transactional
	public PlannerProfile findPlannerProfileById(Long plannerProfileId) {
		return plannerProfileRepository.findById(plannerProfileId)
			.orElseThrow(PlannerProfileNotFoundException::new);
	}

	@Transactional
	public PlannerProfile findPlannerProfileByPlanner(Planner planner) {
		return plannerProfileRepository.findPlannerProfileByPlanner(planner)
			.orElseThrow(PlannerProfileNotFoundException::new);
	}

	/*================== 검증 =================*/
	public void checkIsProfileOwner(Users users, Long plannerProfileId) {
		Planner planner = plannerService.findPlannerByPlannerProfileId(plannerProfileId);

		if (!planner.getUsers().equals(users)) {
			throw new ProfileModificationNotAllowedException();
		}
	}
}
