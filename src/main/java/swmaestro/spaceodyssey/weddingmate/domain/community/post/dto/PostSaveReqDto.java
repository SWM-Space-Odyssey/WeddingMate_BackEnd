package swmaestro.spaceodyssey.weddingmate.domain.community.post.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostSaveReqDto {
	private String title;
	private String content;
	private String category;
}
