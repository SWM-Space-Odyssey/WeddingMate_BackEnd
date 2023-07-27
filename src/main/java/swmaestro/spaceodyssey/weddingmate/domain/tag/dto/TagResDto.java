package swmaestro.spaceodyssey.weddingmate.domain.tag.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class TagResDto {
	Long tagId;
	String content;
	String categoryContent;

	@Builder
	public TagResDto(Long tagId, String content, String categoryContent) {
		this.tagId = tagId;
		this.content = content;
		this.categoryContent = categoryContent;
	}
}
