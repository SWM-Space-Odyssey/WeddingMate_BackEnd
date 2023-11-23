package swmaestro.spaceodyssey.weddingmate.domain.community.comment.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class CommentResDto {
	private Long commentId;
	private String content;
	private String writer;
	private String date;
	private Long userId;
	private String profileImg;
	private Boolean isWriter;

	@Builder
	public CommentResDto(Long id, String content, String writer, String date, Long userId, String profileImg, Boolean isWriter) {
		this.commentId = id;
		this.content = content;
		this.writer = writer;
		this.date = date;
		this.userId = userId;
		this.profileImg = profileImg;
		this.isWriter = isWriter;
	}
}
