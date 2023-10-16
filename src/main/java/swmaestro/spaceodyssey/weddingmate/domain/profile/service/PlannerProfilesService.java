package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.service.LikesRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper.PortfoliosMapper;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfiles;
import swmaestro.spaceodyssey.weddingmate.domain.profile.mapper.ProfilesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannersMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.PlannersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;

@Service
@RequiredArgsConstructor
@Transactional
public class PlannerProfilesService {

	private final UsersRepositoryService usersRepositoryService;
	private final PlannersRepositoryService plannersRepositoryService;
	private final LikesRepositoryService likesRepositoryService;
	private final ProfileRepositoryService profileRepositoryService;

	private final PlannersMapper plannerMapper;
	private final ProfilesMapper profileMapper;
	private final PortfoliosMapper portfoliosMapper;

	public List<PlannerProfileResDto> getPlannerProfileList(Users cUsers) {
		List<Users> plannerUserList = usersRepositoryService.findAllPlannerUser();

		return plannerUserList.parallelStream()
			.map(user -> {
				Planners planners = plannersRepositoryService.findPlannerByUsers(user);
				Boolean isPlannerLiked = likesRepositoryService.isLiked(cUsers, LikeEnum.planner,
					planners.getPlannerId());
				PlannerProfiles plannerProfiles = planners.getPlannerProfiles();
				return profileMapper.toPlannerProfileResDto(user, planners, plannerProfiles, isPlannerLiked);
			})
			.toList();
	}

	public PlannerProfileResDto getPlannerProfileByUserId(Users cUsers, Long userId) {
		Users users = usersRepositoryService.findUserById(userId);
		Planners planners = plannersRepositoryService.findPlannerByUsers(users);
		PlannerProfiles plannerProfiles = profileRepositoryService.findPlannerProfileById(
			planners.getPlannerProfiles().getPlannerProfileId());

		Boolean isPlannerLiked = likesRepositoryService.isLiked(cUsers, LikeEnum.planner, planners.getPlannerId());

		return PlannerProfileResDto.builder()
			.plannerProfileId(plannerProfiles.getPlannerProfileId())
			.nickname(users.getNickname())
			.profileImageUrl(users.getProfileImage().getUrl())
			.plannerInfo(plannerMapper.toPlannerInfoDto(planners))
			.plannerProfileInfo(profileMapper.toPlannerProfileInfoDto(plannerProfiles))
			.isLiked(isPlannerLiked)
			.build();
	}

	public List<PortfolioListResDto> getPlannerPortfolioByProfileId(Users cUsers, Long userId) {
		Users users = usersRepositoryService.findUserById(userId);

		return users.getPortfoliosList().stream()
			//삭제된 portfolio 제외
			.filter(portfolio -> (Boolean.FALSE.equals(portfolio.getIsDeleted())))
			.map(portfolio -> portfoliosMapper.entityToDto(cUsers, portfolio))
			.toList();
	}
}
