package swmaestro.spaceodyssey.weddingmate.domain.profile.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.ProfileImageUrlDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ProfileMapper {

	public PlannerProfileResDto toPlannerProfileResDto(ProfileImageUrlDto profileImageUrlDto, PlannerProfile plannerProfile) {
		return PlannerProfileResDto.builder()
			.plannerProfileId(plannerProfile.getPlannerProfileId())
			.profileImageUrlDto(profileImageUrlDto)
			.bio(plannerProfile.getBio())
			.sns(plannerProfile.getSns())
			.build();
	}

	public PlannerProfileUpdateResDto toPlannerProfileUpdateResDto(PlannerProfile plannerProfile) {
		return PlannerProfileUpdateResDto.builder()
			.sns(plannerProfile.getSns())
			.bio(plannerProfile.getBio())
			.build();
	}
}
