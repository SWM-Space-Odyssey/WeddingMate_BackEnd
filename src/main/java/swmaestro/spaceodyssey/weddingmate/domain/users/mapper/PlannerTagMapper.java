package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.PlannerTag;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PlannerTagMapper {

	public PlannerTag plannerToEntity(Planner planner, Tag tag){
		return PlannerTag.plannerBuilder()
			.planner(planner)
			.tag(tag)
			.build();
	}

	public PlannerTag customerToEntity(Customer customer, Tag tag){
		return PlannerTag.customerBuilder()
			.customer(customer)
			.tag(tag)
			.build();
	}
}
