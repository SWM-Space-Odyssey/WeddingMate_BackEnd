package swmaestro.spaceodyssey.weddingmate.domain.tag.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.dto.CategoryMapper;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagMapper;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagResDto;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.tag.repository.TagRepository;

@Service
@RequiredArgsConstructor
@Transactional
public class TagService {
	private final TagMapper tagMapper;
	private final TagRepository tagRepository;
	private final CategoryMapper categoryMapper;

	public List<TagResDto> findAllTag(String categoryContent) {
		Category category = categoryMapper.findCategoryByContentOrElseCreate(categoryContent);
		List<Tag> tagList = tagRepository.findByIsDefaultAndCategory(true, category);
		return tagList.stream().map(tagMapper::entityToDto).toList();
	}
}
