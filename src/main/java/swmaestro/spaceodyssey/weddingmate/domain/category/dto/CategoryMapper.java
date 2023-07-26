package swmaestro.spaceodyssey.weddingmate.domain.category.dto;

import java.util.Optional;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.category.repository.CategoryRepository;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class CategoryMapper {
	private final CategoryRepository categoryRepository;
	public CategoryResDto entityToDto(Category category) {
		return CategoryResDto.builder().content(category.getContent()).build();
	}

	public Category findCategoryByContentOrElseCreate(String content) {
		Optional<Category> categoryOptional = categoryRepository.findByContent(content);

		if (categoryOptional.isEmpty()) {
			Category category = Category.builder().content(content).isDefault(false).build();
			categoryRepository.save(category);

			return category;
		}
		return categoryOptional.get();
	}
}
