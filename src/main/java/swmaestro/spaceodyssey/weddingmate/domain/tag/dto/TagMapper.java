package swmaestro.spaceodyssey.weddingmate.domain.tag.dto;

import java.util.Optional;

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
		Optional<Tag> tagOptional = tagRepository.findByContent(keyword);

		if (tagOptional.isEmpty()) {
			Tag tag = Tag.builder().content(keyword).category(category).isDefault(false).build();
			tagRepository.save(tag);

			return tag;
		}
		return tagOptional.get();
	}

	public TagResDto entityToDto(Tag tag) {

		return TagResDto.builder()
			.tagId(tag.getTagId())
			.content(tag.getContent())
			.categoryContent(tag.getCategory().getContent())
			.build();
	}
}
