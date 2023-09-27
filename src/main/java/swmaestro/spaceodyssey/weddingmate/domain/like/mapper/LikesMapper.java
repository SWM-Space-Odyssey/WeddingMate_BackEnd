package swmaestro.spaceodyssey.weddingmate.domain.like.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.CompanyLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PlannerLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.PortfolioLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannersRepository;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class LikesMapper {

	public final PortfoliosRepository portfoliosRepository;
	public final ItemsRepository itemsRepository;
	public final PlannersRepository plannersRepository;

	public PortfolioLikeResDto entityToDto(Portfolios portfolios) {

		return PortfolioLikeResDto.builder()
			.id(portfolios.getPortfolioId())
			.title(portfolios.getTitle())
			.repImgUrl(portfolios.getFiles().getUrl())
			.build();
	}

	public PortfolioLikeResDto entityToDto(Items items) {

		return PortfolioLikeResDto.builder()
			.id(items.getItemId())
			.title(items.getItemRecord())
			.repImgUrl(items.getFilesList().get(0).getUrl())
			.build();
	}

	public PlannerLikeResDto entityToDto(Planners planners) {

		return PlannerLikeResDto.builder()
			.id(planners.getPlannerId())
			.name(planners.getUsers().getNickname())
			.profileImg(planners.getUsers().getProfileImage().getUrl())
			.build();
	}

	public CompanyLikeResDto entityToDto(Companies companies) {

		return CompanyLikeResDto.builder()
			.id(companies.getCompanyId())
			.name(companies.getName())
			.address(companies.getAddress())
			//.repImgUrl(companies.getFiles().getUrl())
			.build();
	}
}
