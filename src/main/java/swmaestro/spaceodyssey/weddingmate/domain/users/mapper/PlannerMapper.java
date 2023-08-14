package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerMapper {

	public Planner toEntity(PlannerSignupReqDto reqDto) {

		return Planner.builder()
			.company(reqDto.getCompany())
			.position(reqDto.getPosition())
			.region(reqDto.getRegion())
			.plannerTagList(reqDto.getPlannerTagList())
			.build();
	}
}
