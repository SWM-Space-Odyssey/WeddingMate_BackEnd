package swmaestro.spaceodyssey.weddingmate.domain.company.mapper;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.dto.CompanyItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.company.dto.CompanySearchResDto;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class CompanyMapper {
		private final FilesRepository filesRepository;
		private final LikesRepository likesRepository;

	public List<String> entityToImageDtoByLimit(Companies company) {

		return company.getItems()
			.stream()
			.flatMap(item -> filesRepository.findByItems(item).stream())
			.map(Files::getUrl)
			.limit(10)
			.toList();
	}

	public List<String> entityToImageDto(Companies company) {

		return company.getItems()
			.stream()
			.flatMap(item -> filesRepository.findByItems(item).stream())
			.map(Files::getUrl)
			.toList();
	}

	public CompanyItemResDto entityToItemDto(Items item) {

		return CompanyItemResDto.builder()
			.itemRecord(item.getItemRecord())
			.repImgUrl(item.getFilesList().get(0).getUrl())
			.userId(item.getPortfolios().getUsers().getUserId())
			.itemDate(item.getItemDate())
			.build();
	}

	public CompanySearchResDto entityToSearchDto(Companies company, Users user) {

		Boolean isLiked = isCompanyLikedByUser(user, company.getCompanyId());

		return CompanySearchResDto.builder()
			.companyId(company.getCompanyId())
			.name(company.getName())
			.category(company.getCategory())
			.repImg(company.getFiles().getUrl())
			.isLiked(isLiked)
			.build();
	}

	private boolean isCompanyLikedByUser(Users users, Long companyId) {
		return !likesRepository.findByUsersAndLikeTypeAndLikedId(users, LikeEnum.company, companyId).isEmpty();
	}

}
