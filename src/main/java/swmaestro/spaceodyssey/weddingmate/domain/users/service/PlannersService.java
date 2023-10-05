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
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;

@Transactional
@Service
@RequiredArgsConstructor
public class PlannersService {

	private final UsersService usersService;
	private final UsersRepositoryService usersRepositoryService;

	private final PlannersMapper plannerMapper;
	private final PlannersRepository plannersRepository;

	@Transactional
	public void signupPlanner(Users users, PlannerSignupReqDto reqDto) {
		Users pUsers = usersRepositoryService.findUserByEmail(users.getEmail());
		usersService.checkUserIsRegistered(users);

		pUsers.updateNickname(reqDto.getNickname());
		pUsers.updateRegisterStatus(UserRegisterStatusEnum.PLANNER);

		Planners planners = createPlanner(pUsers, reqDto);
		createPlannerProfile(planners); // JPA에 의해 자동으로 planner에 반영

		plannersRepository.save(planners);
	}

	@Transactional
	public Planners createPlanner(Users users, PlannerSignupReqDto reqDto) {
		Planners planners = plannerMapper.plannerSignupReqDtoToEntity(reqDto);
		planners.setUsers(users);
		return planners;
	}

	@Transactional
	public void createPlannerProfile(Planners planners) {
		PlannerProfiles plannerProfiles = PlannerProfiles.builder()
			.bio("반갑습니다.")
			.build();
		plannerProfiles.setPlanners(planners);
	}
}
