package swmaestro.spaceodyssey.weddingmate.domain.community.post.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostListResDto {
	private Long postId;
	private String title;
	private String category;
	private String date;
	private String writer;
	private String content;
	private Integer commentCnt;

	@Builder
	public PostListResDto(Long postId, String title, String category, String date, String writer, String content, Integer commentCnt) {
		this.postId = postId;
		this.title = title;
		this.category = category;
		this.date = date;
		this.writer = writer;
		this.content = content;
		this.commentCnt = commentCnt;
	}
}
