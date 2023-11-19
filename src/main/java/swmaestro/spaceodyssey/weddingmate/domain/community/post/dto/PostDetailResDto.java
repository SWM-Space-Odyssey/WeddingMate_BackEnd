package swmaestro.spaceodyssey.weddingmate.domain.community.post.dto;

import java.util.List;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.dto.CommentResDto;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostDetailResDto {
	private String title;
	private String category;
	private String date;
	private String writer;
	private String profileImg;
	private Long userId;
	private String content;
	private List<CommentResDto> commentList;
	private Boolean isWriter;

	@Builder
	public PostDetailResDto(String title, String category, String date, String writer,
		String profileImg, Long userId, String content, List<CommentResDto> commentList, Boolean isWriter) {
		this.title = title;
		this.category = category;
		this.writer = writer;
		this.date = date;
		this.profileImg = profileImg;
		this.userId = userId;
		this.content = content;
		this.commentList = commentList;
		this.isWriter = isWriter;
	}
}
