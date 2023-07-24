package swmaestro.spaceodyssey.weddingmate.domain.tag.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class TagResDto {
	Long tagId;
	String content;

	@Builder
	public TagResDto(Long tagId, String content){
		this.tagId = tagId;
		this.content = content;
	}
}
