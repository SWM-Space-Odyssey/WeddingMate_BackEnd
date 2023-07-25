package swmaestro.spaceodyssey.weddingmate.domain.category.service;

import java.util.List;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.dto.CategoryMapper;
import swmaestro.spaceodyssey.weddingmate.domain.category.dto.CategoryResDto;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.category.repository.CategoryRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.category.CategoryNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class CategoryService {

	private final CategoryRepository categoryRepository;
	private final CategoryMapper categoryMapper;

	public List<CategoryResDto> findAllCategory() {
		List<Category> categoryList = categoryRepository.findByIsDefault(true);
		return categoryList.stream().map(categoryMapper::entityToDto).toList();
	}

	public Category findById(Long id) {
		return categoryRepository.findById(id)
			.orElseThrow(CategoryNotFoundException::new);
	}
}
