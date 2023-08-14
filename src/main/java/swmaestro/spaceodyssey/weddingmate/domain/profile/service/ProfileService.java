package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileReqDto;
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
import swmaestro.spaceodyssey.weddingmate.domain.users.service.UsersService;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.PlannerProfileNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.ProfileModificationNotAllowedException;

@Service
@RequiredArgsConstructor
@Transactional
public class ProfileService {

	private final PlannerProfileRepository plannerProfileRepository;

	private final PlannerMapper plannerMapper;
	private final ProfileMapper profileMapper;

	private final UsersService usersService;
	private final PlannerService plannerService;

	public List<PlannerProfileResDto> getPlannerProfileList() {
		List<Users> plannerUserList = usersService.findAllPlannerUser();

		return plannerUserList.parallelStream()
			.map(user -> {
				Planner planner = plannerService.findPlannerByUser(user);
				PlannerProfile plannerProfile = findPlannerProfileByPlanner(planner);
				return profileMapper.toPlannerProfileResDto(user, planner, plannerProfile);
			})
			.toList();
	}

	public PlannerProfileResDto getPlannerProfileByProfileId(PlannerProfileReqDto plannerProfileReqDto) {
		PlannerProfile plannerProfile = findPlannerProfileById((plannerProfileReqDto.getPlannerProfileId()));
		Planner planner = plannerService.findPlannerByPlannerProfileId(plannerProfile.getPlannerProfileId());
		Users users = usersService.findUserByPlanner(planner);

		return PlannerProfileResDto.builder()
			.nickname(users.getNickname())
			.profileImageUrl(users.getProfileImage().getUrl())
			.plannerInfo(plannerMapper.toPlannerInfoDto(planner))
			.plannerProfileInfo(profileMapper.toPlannerProfileInfoResDto(plannerProfile))
			.build();
	}

	@Transactional
	public PlannerProfileUpdateResDto updatePlannerProfile(Users users, PlannerProfileUpdateReqDto reqDto) {
		checkIsProfileOwner(users, reqDto.getPlannerProfileId());

		PlannerProfile plannerProfile = findPlannerProfileById(reqDto.getPlannerProfileId());

		if (reqDto.getBio() != null) {
			plannerProfile.updateBio(reqDto.getBio());
		}
		if (reqDto.getSns() != null) {
			plannerProfile.updateSns(reqDto.getSns());
		}

		return profileMapper.toPlannerProfileUpdateResDto(plannerProfile);
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
