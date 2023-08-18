package swmaestro.spaceodyssey.weddingmate.domain.profile.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.mapper.PlannerMapper;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ProfileMapper {

	private final PlannerMapper plannerMapper;

	public PlannerProfileInfoDto toPlannerProfileInfoDto(PlannerProfile plannerProfile) {
		return PlannerProfileInfoDto.builder()
			.bio(plannerProfile.getBio())
			.sns(plannerProfile.getSns())
			.build();
	}

	public PlannerProfileResDto toPlannerProfileResDto(Users user, Planner planner, PlannerProfile plannerProfile) {
		return PlannerProfileResDto.builder()
			.plannerProfileId(plannerProfile.getPlannerProfileId())
			.nickname(user.getNickname())
			.profileImageUrl(user.getProfileImage().getUrl())
			.plannerInfo(plannerMapper.toPlannerInfoDto(planner))
			.plannerProfileInfo(toPlannerProfileInfoDto(plannerProfile))
			.build();
	}

	public PlannerProfileUpdateResDto toPlannerProfileUpdateResDto(String nickname, Planner planner,
		PlannerProfile plannerProfile) {
		return PlannerProfileUpdateResDto.builder()
			.nickname(nickname)
			.plannerInfo(plannerMapper.toPlannerInfoDto(planner))
			.plannerProfileInfo(toPlannerProfileInfoDto(plannerProfile))
			.build();
	}
}
