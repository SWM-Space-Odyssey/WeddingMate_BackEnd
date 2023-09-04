package swmaestro.spaceodyssey.weddingmate.domain.profile.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfiles;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannersMapper;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ProfilesMapper {

	private final PlannersMapper plannerMapper;

	public PlannerProfileInfoDto toPlannerProfileInfoDto(PlannerProfiles plannerProfiles) {
		return PlannerProfileInfoDto.builder()
			.bio(plannerProfiles.getBio())
			.sns(plannerProfiles.getSns())
			.build();
	}

	public PlannerProfileResDto toPlannerProfileResDto(Users user, Planners planners, PlannerProfiles plannerProfiles, Boolean isLiked) {
		return PlannerProfileResDto.builder()
			.plannerProfileId(plannerProfiles.getPlannerProfileId())
			.nickname(user.getNickname())
			.profileImageUrl(user.getProfileImage().getUrl())
			.plannerInfo(plannerMapper.toPlannerInfoDto(planners))
			.plannerProfileInfo(toPlannerProfileInfoDto(plannerProfiles))
			.isLiked(isLiked)
			.build();
	}

	public PlannerProfileUpdateResDto toPlannerProfileUpdateResDto(String nickname, Planners planners,
		PlannerProfiles plannerProfiles) {
		return PlannerProfileUpdateResDto.builder()
			.nickname(nickname)
			.plannerInfo(plannerMapper.toPlannerInfoDto(planners))
			.plannerProfileInfo(toPlannerProfileInfoDto(plannerProfiles))
			.build();
	}
}
