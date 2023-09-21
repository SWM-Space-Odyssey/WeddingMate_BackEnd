package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper.PortfoliosMapper;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfiles;
import swmaestro.spaceodyssey.weddingmate.domain.profile.mapper.ProfilesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannersMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.PlannersService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.UsersService;

@Service
@RequiredArgsConstructor
@Transactional
public class PlannerProfilesService {

	private final UsersService usersService;
	private final PlannersService plannerService;
	private final ProfilesService profilesService;

	private final PlannersMapper plannerMapper;
	private final ProfilesMapper profileMapper;
	private final PortfoliosMapper portfoliosMapper;

	private final LikesRepository likeRepository;


	public List<PlannerProfileResDto> getPlannerProfileList(Users cUsers) {
		List<Users> plannerUserList = usersService.findAllPlannerUser();

		return plannerUserList.parallelStream()
			.map(user -> {
				Planners planners = plannerService.findPlannerByUser(user);
				Boolean isPlannerLiked = isLiked(cUsers, LikeEnum.PLANNER, planners.getPlannerId());
				PlannerProfiles plannerProfiles = planners.getPlannerProfiles();
				return profileMapper.toPlannerProfileResDto(user, planners, plannerProfiles, isPlannerLiked);
			})
			.toList();
	}

	public PlannerProfileResDto getPlannerProfileByProfileId(Users cUsers, Long plannerProfileId) {
		PlannerProfiles plannerProfiles = profilesService.findPlannerProfileById(plannerProfileId);
		Planners planners = plannerService.findPlannerByPlannerProfileId(plannerProfiles.getPlannerProfileId());
		Users userByPlanner = usersService.findUserByPlanner(planners);

		Boolean isPlannerLiked = isLiked(cUsers, LikeEnum.PLANNER, planners.getPlannerId());

		return PlannerProfileResDto.builder()
			.plannerProfileId(plannerProfiles.getPlannerProfileId())
			.nickname(userByPlanner.getNickname())
			.profileImageUrl(userByPlanner.getProfileImage().getUrl())
			.plannerInfo(plannerMapper.toPlannerInfoDto(planners))
			.plannerProfileInfo(profileMapper.toPlannerProfileInfoDto(plannerProfiles))
			.isLiked(isPlannerLiked)
			.build();
	}

	public List<PortfolioListResDto> getPlannerPortfolioByProfileId(Users cUsers, Long userId) {
		Users users = usersService.findUserById(userId);

		return users.getPortfoliosList().stream()
			//삭제된 portfolio 제외
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(portfolio -> portfoliosMapper.entityToDto(cUsers, portfolio))
			.toList();
	}


	/*================== Repository 접근 ==================*/
	public Boolean isLiked(Users cUsers, LikeEnum likeType, Long likeId) {
		return !likeRepository.findByUsersAndLikeTypeAndLikedId(cUsers, likeType, likeId).isEmpty();
	}
}
