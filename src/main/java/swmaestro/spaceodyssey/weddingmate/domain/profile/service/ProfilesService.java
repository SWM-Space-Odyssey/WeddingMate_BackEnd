package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.CustomerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.CustomerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfiles;
import swmaestro.spaceodyssey.weddingmate.domain.profile.mapper.ProfilesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.profile.repository.PlannerProfilesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.CustomersMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannersMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.CustomersService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.PlannersService;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.PlannerProfileNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.ProfileModificationNotAllowedException;

@Service
@RequiredArgsConstructor
@Transactional
public class ProfilesService {

	private final PlannerProfilesRepository plannerProfilesRepository;

	private final PlannersMapper plannerMapper;
	private final ProfilesMapper profileMapper;
	private final CustomersMapper customersMapper;

	private final PlannersService plannerService;
	private final CustomersService customersService;
	private final UsersRepository usersRepository;

	public PlannerProfileResDto getPlannerProfile(Users users) {
		Planners planners = plannerService.findPlannerByUser(users);
		PlannerProfiles plannerProfiles = planners.getPlannerProfiles(); // EAGER로 인해 즉시 로딩 가능

		return PlannerProfileResDto.builder()
			.plannerProfileId(plannerProfiles.getPlannerProfileId())
			.nickname(users.getNickname())
			.profileImageUrl(users.getProfileImage().getUrl())
			.plannerInfo(plannerMapper.toPlannerInfoDto(planners))
			.plannerProfileInfo(profileMapper.toPlannerProfileInfoDto(plannerProfiles))
			.build();
	}

	@Transactional
	public PlannerProfileUpdateResDto updatePlannerProfile(Users users, PlannerProfileUpdateReqDto reqDto) {
		Planners planners = plannerService.findPlannerByUser(users);
		PlannerProfiles plannerProfiles = planners.getPlannerProfiles(); // EAGER로 인해 즉시 로딩 가능

		// 변경사항 수정
		users.updateNickname(reqDto.getNickname());
		usersRepository.save(users);

		planners.updatePlannerInfo(reqDto.getPlannerInfo());
		plannerProfiles.updatePlannerProfileInfo(reqDto.getPlannerProfileInfo());

		return profileMapper.toPlannerProfileUpdateResDto(users.getNickname(), planners, plannerProfiles);
	}

	@Transactional
	public CustomerProfileResDto getCustomerProfile(Users users) {
		Customers customers = customersService.findCustomerByUser(users);

		return CustomerProfileResDto.builder()
			.userId(users.getUserId())
			.nickname(users.getNickname())
			.profileImageUrl(users.getProfileImage().getUrl())
			.customerTagListDto(customersMapper.toCustomerTagListDto(customers))
			.customerInfoDto(customersMapper.toCustomerInfoDto(customers))
			.build();

	}

	@Transactional
	public void updateCustomerProfile(Users users, CustomerProfileUpdateReqDto reqDto) {
		Customers customers = customersService.findCustomerByUser(users);

		// 변경사항 수정
		users.updateNickname(reqDto.getNickname());
		usersRepository.save(users);

		customers.updateCustomerInfo(reqDto.getCustomerInfo());
		customers.updateCustomerTagList(reqDto.getCustomerTagList());
	}

	/*================== Repository 접근 ==================*/
	@Transactional
	public PlannerProfiles findPlannerProfileById(Long plannerProfileId) {
		return plannerProfilesRepository.findById(plannerProfileId)
			.orElseThrow(PlannerProfileNotFoundException::new);
	}

	@Transactional
	public PlannerProfiles findPlannerProfileByPlanner(Planners planners) {
		return plannerProfilesRepository.findPlannerProfilesByPlanners(planners)
			.orElseThrow(PlannerProfileNotFoundException::new);
	}

	/*================== 검증 =================*/
	public void checkIsProfileOwner(Users users, Long plannerProfileId) {
		Planners planners = plannerService.findPlannerByPlannerProfileId(plannerProfileId);

		if (!planners.getUsers().equals(users)) {
			throw new ProfileModificationNotAllowedException();
		}
	}
}
