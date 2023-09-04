package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerMapper {

	public Planner plannerSignupReqDtoToEntity(PlannerSignupReqDto reqDto) {

		return Planner.builder()
			.company(reqDto.getCompany())
			.position(reqDto.getPosition())
			.regionList(reqDto.getRegion())
			.plannerTagList(reqDto.getPlannerTagList())
			.build();
	}

	public PlannerInfoDto toPlannerInfoDto(Planner planner) {
		return PlannerInfoDto.builder()
			.company(planner.getCompany())
			.position(planner.getPosition())
			.regionList(planner.getRegionList())
			.tagList(planner.getPlannerTagList())
			.build();
	}
}
