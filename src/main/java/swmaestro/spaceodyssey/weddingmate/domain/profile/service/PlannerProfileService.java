package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;
import swmaestro.spaceodyssey.weddingmate.domain.profile.mapper.ProfileMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannerMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.PlannerService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.UsersService;

@Service
@RequiredArgsConstructor
@Transactional
public class PlannerProfileService {

	private final UsersService usersService;
	private final PlannerService plannerService;
	private final ProfileService profileService;

	private final PlannerMapper plannerMapper;
	private final ProfileMapper profileMapper;

	public List<PlannerProfileResDto> getPlannerProfileList() {
		List<Users> plannerUserList = usersService.findAllPlannerUser();

		return plannerUserList.parallelStream()
			.map(user -> {
				Planner planner = plannerService.findPlannerByUser(user);
				PlannerProfile plannerProfile = profileService.findPlannerProfileByPlanner(planner);
				return profileMapper.toPlannerProfileResDto(user, planner, plannerProfile);
			})
			.toList();
	}

	public PlannerProfileResDto getPlannerProfileByProfileId(Long plannerProfileId) {
		PlannerProfile plannerProfile = profileService.findPlannerProfileById(plannerProfileId);
		Planner planner = plannerService.findPlannerByPlannerProfileId(plannerProfile.getPlannerProfileId());
		Users users = usersService.findUserByPlanner(planner);

		return PlannerProfileResDto.builder()
			.nickname(users.getNickname())
			.profileImageUrl(users.getProfileImage().getUrl())
			.plannerInfo(plannerMapper.toPlannerInfoDto(planner))
			.plannerProfileInfo(profileMapper.toPlannerProfileInfoResDto(plannerProfile))
			.build();
	}
}
