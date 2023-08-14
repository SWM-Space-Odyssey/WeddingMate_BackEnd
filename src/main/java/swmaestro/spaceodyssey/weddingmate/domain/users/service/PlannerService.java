package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannerMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannerRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerNotFoundException;

@Transactional
@Service
@RequiredArgsConstructor
public class PlannerService {

	private final UsersService usersService;
	private final PlannerMapper plannerMapper;
	private final PlannerRepository plannerRepository;

	@Transactional
	public void signupPlanner(Users users, PlannerSignupReqDto reqDto) {
		Users pUsers = usersService.findUserByEmail(users.getEmail());
		usersService.checkUserIsRegistered(users);

		pUsers.updateNickname(reqDto.getNickname());
		pUsers.updateRegisterStatus(UserRegisterStatusEnum.PLANNER);

		Planner planner = createPlanner(pUsers, reqDto);
		PlannerProfile plannerProfile = createPlannerProfile(planner); // JPA에 의해 자동으로 planner에 반영

		plannerRepository.save(planner);
	}

	@Transactional
	public Planner createPlanner(Users users, PlannerSignupReqDto reqDto) {
		Planner planner = plannerMapper.plannerSignupReqDtoToEntity(reqDto);
		planner.setUsers(users);
		return planner;
	}

	@Transactional
	public PlannerProfile createPlannerProfile(Planner planner) {
		PlannerProfile plannerProfile = PlannerProfile.builder()
			.bio("반갑습니다.")
			.build();
		plannerProfile.setPlanner(planner);
		return plannerProfile;
	}

	/*================== Repository 접근 ==================*/
	@Transactional
	public Planner findPlannerByUser(Users users) {
		return plannerRepository.findByUsers(users)
			.orElseThrow(PlannerNotFoundException::new);
	}

	@Transactional
	public Planner findPlannerByPlannerProfileId(Long plannerProfileId){
		return plannerRepository.findByPlannerProfile_PlannerProfileId(plannerProfileId)
			.orElseThrow(PlannerNotFoundException::new);
	}
}
