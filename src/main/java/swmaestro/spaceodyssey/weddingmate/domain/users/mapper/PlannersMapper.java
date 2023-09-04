package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannersMapper {

	public Planners plannerSignupReqDtoToEntity(PlannerSignupReqDto reqDto) {

		return Planners.builder()
			.company(reqDto.getCompany())
			.position(reqDto.getPosition())
			.regionList(reqDto.getRegionList())
			.plannerTagList(reqDto.getPlannerTagList())
			.build();
	}

	public PlannerInfoDto toPlannerInfoDto(Planners planners) {
		return PlannerInfoDto.builder()
			.company(planners.getCompany())
			.position(planners.getPosition())
			.regionList(planners.getRegionList())
			.tagList(planners.getPlannerTagList())
			.build();
	}
}
