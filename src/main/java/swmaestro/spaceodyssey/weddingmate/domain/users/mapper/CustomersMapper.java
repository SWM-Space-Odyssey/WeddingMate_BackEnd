package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.CustomerInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerTagListDto;
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

	public CustomerTagListDto toCustomerTagListDto(Customers customers) {

		return CustomerTagListDto.builder()
			.dressTagList(customers.getDressTagList())
			.makeupTagList(customers.getMakeupTagList())
			.plannerTagList(customers.getPlannerTagList())
			.portfolioTagList(customers.getPortfolioTagList())
			.studioFocusTagList(customers.getStudioFocusTagList())
			.studioTypeTagList(customers.getStudioTypeTagList())
			.build();
	}

	public CustomerInfoDto toCustomerInfoDto(Customers customers) {

		return CustomerInfoDto.builder()
			.weddingDateConfirmed(customers.getWeddingDateConfirmed())
			.weddingDate(customers.getWeddingDate())
			.budget(customers.getBudget())
			.regionList(customers.getRegionList())
			.build();
	}
}

