package swmaestro.spaceodyssey.weddingmate.domain.tag.dto;

import java.util.Optional;
import java.util.List;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.tag.repository.TagRepository;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class TagMapper {
	private final TagRepository tagRepository;

	public Tag contentToEntity(String keyword, Category category) {
		return tagRepository.findByContent(keyword)
			.orElseGet(() -> tagRepository.save(
				Tag.builder()
					.content(keyword)
					.category(category)
					.isDefault(false)
					.build()
			));
	}

	public TagResDto entityToDto(Tag tag) {

		return TagResDto.builder()
			.tagId(tag.getTagId())
			.content(tag.getContent())
			.build();
	}
}
