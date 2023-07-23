package swmaestro.spaceodyssey.weddingmate.domain.tag.dto;

import java.util.Optional;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.tag.repository.TagRepository;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class TagMapper {
	private final TagRepository tagRepository;
	public Tag contentToEntity(String keyword) {
		Optional<Tag> tagOptional = tagRepository.findByContent(keyword);
		return tagOptional.orElse(null);
		//ToDo: 기존에 없던 태그 입력 시 생성
	}

	public TagResDto entityToDto(Tag tag) {

		return TagResDto.builder()
			.tagId(tag.getTagId())
			.content(tag.getContent())
			.build();
	}
}
