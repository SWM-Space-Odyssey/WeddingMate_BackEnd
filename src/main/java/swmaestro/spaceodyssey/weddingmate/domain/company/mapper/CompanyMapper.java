package swmaestro.spaceodyssey.weddingmate.domain.company.mapper;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.dto.CompanyItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.like.repository.LikesRepository;

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

}
