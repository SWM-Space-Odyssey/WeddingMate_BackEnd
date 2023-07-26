package swmaestro.spaceodyssey.weddingmate.domain.category.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class CategoryResDto {
	private String content;

	@Builder
	public CategoryResDto(String content) {
		this.content = content;
	}
}
