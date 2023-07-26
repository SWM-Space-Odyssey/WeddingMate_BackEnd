package swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.PortfolioTag;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PortfolioTagMapper {

	public PortfolioTag customerToEntity(Customer customer, Tag tag) {
		return PortfolioTag.customerBuilder()
			.customer(customer)
			.tag(tag)
			.build();
	}
}
