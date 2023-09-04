package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class CustomersMapper {

	public Customers toEntity(CustomerSignupReqDto reqDto) {
		Customers customers =  Customers.builder()
			.weddingDateConfirmed(reqDto.getWeddingDateConfirmed())
			.regionList(reqDto.getRegionList())
			.budget(reqDto.getBudget())
			.customerTagList(reqDto.getCustomerTagList())
			.build();

		if (Boolean.TRUE.equals(reqDto.getWeddingDateConfirmed())) {
			customers.setWeddingDate(reqDto.getWeddingDate());
		}
		return customers;
	}
}

