package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfiles;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannersMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerNotFoundException;

@Transactional
@Service
@RequiredArgsConstructor
public class PlannersService {

	private final UsersService usersService;
	private final PlannersMapper plannerMapper;
	private final PlannersRepository plannersRepository;

	@Transactional
	public void signupPlanner(Users users, PlannerSignupReqDto reqDto) {
		Users pUsers = usersService.findUserByEmail(users.getEmail());
		usersService.checkUserIsRegistered(users);

		pUsers.updateNickname(reqDto.getNickname());
		pUsers.updateRegisterStatus(UserRegisterStatusEnum.PLANNER);

		Planners planners = createPlanner(pUsers, reqDto);
		PlannerProfiles plannerProfiles = createPlannerProfile(planners); // JPA에 의해 자동으로 planner에 반영

		plannersRepository.save(planners);
	}

	@Transactional
	public Planners createPlanner(Users users, PlannerSignupReqDto reqDto) {
		Planners planners = plannerMapper.plannerSignupReqDtoToEntity(reqDto);
		planners.setUsers(users);
		return planners;
	}

	@Transactional
	public PlannerProfiles createPlannerProfile(Planners planners) {
		PlannerProfiles plannerProfiles = PlannerProfiles.builder()
			.bio("반갑습니다.")
			.build();
		plannerProfiles.setPlanners(planners);
		return plannerProfiles;
	}

	/*================== Repository 접근 ==================*/
	@Transactional
	public Planners findPlannerByUser(Users users) {
		return plannersRepository.findByUsers(users)
			.orElseThrow(PlannerNotFoundException::new);
	}

	@Transactional
	public Planners findPlannerByPlannerProfileId(Long plannerProfileId){
		return plannersRepository.findByPlannerProfiles_PlannerProfileId(plannerProfileId)
			.orElseThrow(PlannerNotFoundException::new);
	}
}
