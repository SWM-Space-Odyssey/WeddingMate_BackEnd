package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import java.util.List;
import java.util.Objects;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.CategoryEnum;
import swmaestro.spaceodyssey.weddingmate.domain.category.dto.CategoryMapper;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagMapper;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.PlannerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.PlannerTag;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerMapper {
	private final CategoryMapper categoryMapper;
	private final TagMapper tagMapper;
	private final PlannerTagMapper plannerTagMapper;

	public Planner toEntity(Users users, PlannerSignupReqDto reqDto) {
		Planner planner = Planner.builder()
			.company(reqDto.getCompany())
			.position(reqDto.getPosition())
			.region(reqDto.getRegion())
			.build();
		planner.setUsers(users);

		Category category = categoryMapper.findCategoryByContentOrElseCreate(CategoryEnum.PLANNER.name());
		List<Tag> tagList = reqDto.getPlannerTagList()
			.stream().filter(Objects::nonNull)
			.map(content -> tagMapper.contentToEntity(content, category))
			.toList();
		List<PlannerTag> plannerTagList = tagList
			.stream().filter(Objects::nonNull)
			.map(tag -> plannerTagMapper.plannerToEntity(planner, tag))
			.toList();

		planner.createPlannerTagList(plannerTagList);
		return planner;
	}
}
