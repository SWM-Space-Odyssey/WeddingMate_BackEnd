package swmaestro.spaceodyssey.weddingmate.domain.profile.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileUpdateResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ProfileMapper {

	public PlannerProfileResDto toPlannerProfileResDto(String profileImageUrl, PlannerProfile plannerProfile) {
		return PlannerProfileResDto.builder()
			.plannerProfileId(plannerProfile.getPlannerProfileId())
			.profileImageUrl(profileImageUrl)
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
