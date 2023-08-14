package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class CustomerMapper {

	public Customer toEntity(CustomerSignupReqDto reqDto) {
		Customer customer =  Customer.builder()
			.weddingDateConfirmed(reqDto.getWeddingDateConfirmed())
			.region(reqDto.getRegion())
			.budget(reqDto.getBudget())
			.customerTagList(reqDto.getCustomerTagList())
			.build();

		if (Boolean.TRUE.equals(reqDto.getWeddingDateConfirmed())) {
			customer.setWeddingDate(reqDto.getWeddingDate());
		}
		return customer;
	}
}

