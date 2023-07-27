package swmaestro.spaceodyssey.weddingmate.domain.users.mapper;

import java.util.List;
import java.util.Objects;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.CategoryEnum;
import swmaestro.spaceodyssey.weddingmate.domain.category.dto.CategoryMapper;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioTagMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.PortfolioTag;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagMapper;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerSignupReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.PlannerTag;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class CustomerMapper {
	private final CategoryMapper categoryMapper;
	private final TagMapper tagMapper;
	private final PortfolioTagMapper portfolioTagMapper;
	private final PlannerTagMapper plannerTagMapper;

	public Customer toEntity(CustomerSignupReqDto reqDto) {
		Customer customer = Customer.builder()
			.weddingDate(reqDto.getWeddingDate())
			.budget(reqDto.getBudget())
			.build();

		List<PortfolioTag> portfolioTagList = getPortfolioTagList(customer, reqDto.getPortfolioTagList());
		List<PlannerTag> plannerTagList = getPlannerTagList(customer, reqDto.getPlannerTagList());

		customer.createPortfolioTagList(portfolioTagList);
		customer.createPlannerTagList(plannerTagList);

		return customer;
	}

	public List<PortfolioTag> getPortfolioTagList(Customer customer, List<String> portfolioTagListReqDtO) {
		Category portfolioCategory = categoryMapper.findCategoryByContentOrElseCreate(CategoryEnum.PORTFOLIO.name());
		List<Tag> tagList = portfolioTagListReqDtO
			.stream().filter(Objects::nonNull)
			.map(content -> tagMapper.contentToEntity(content, portfolioCategory))
			.toList();
		return tagList
			.stream().filter(Objects::nonNull)
			.map(tag -> portfolioTagMapper.customerToEntity(customer, tag))
			.toList();
	}

	public List<PlannerTag> getPlannerTagList(Customer customer, List<String> plannerTagListReqDto) {
		Category portfolioCategory = categoryMapper.findCategoryByContentOrElseCreate(CategoryEnum.PLANNER.name());
		List<Tag> tagList = plannerTagListReqDto
			.stream().filter(Objects::nonNull)
			.map(content -> tagMapper.contentToEntity(content, portfolioCategory))
			.toList();
		return tagList
			.stream().filter(Objects::nonNull)
			.map(tag -> plannerTagMapper.customerToEntity(customer, tag))
			.toList();
	}
}
