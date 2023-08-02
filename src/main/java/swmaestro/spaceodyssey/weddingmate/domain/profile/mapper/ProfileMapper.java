package swmaestro.spaceodyssey.weddingmate.domain.profile.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileResDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ProfileMapper {

	public PlannerProfileResDto toPlannerProfileResDto(PlannerProfile plannerProfile) {
		return PlannerProfileResDto.builder()
			.plannerProfileId(plannerProfile.getPlannerProfileId())
			.bio(plannerProfile.getBio())
			.sns(plannerProfile.getSns())
			.build();
	}
}
