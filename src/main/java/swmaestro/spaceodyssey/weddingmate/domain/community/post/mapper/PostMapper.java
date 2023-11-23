package swmaestro.spaceodyssey.weddingmate.domain.community.post.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.mapper.CommentMapper;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.entity.Posts;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class PostMapper {
	private final CommentMapper commentMapper;

	public Posts dtoToEntity(Users user, PostSaveReqDto dto) {

		return Posts.builder()
			.title(dto.getTitle())
			.category(dto.getCategory())
			.content(dto.getContent())
			.user(user)
			.build();
	}

	public PostDetailResDto entityToDto(Posts post, Boolean isWriter) {

		return PostDetailResDto.builder()
			.profileImg(post.getUsers().getProfileImage().getUrl())
			.content(post.getContent())
			.userId(post.getUsers().getUserId())
			.category(post.getCategory())
			.title(post.getTitle())
			.date(post.getCreatedOn().toString())
			.writer(post.getUsers().getNickname())
			.commentList(post.getComments().stream().map(commentMapper::entityToDto).toList())
			.isWriter(isWriter)
			.build();
	}

	public PostListResDto entityToListDto(Posts post) {

		return PostListResDto.builder()
			.postId(post.getPostId())
			.title(post.getTitle())
			.category(post.getCategory())
			.content(post.getContent())
			.commentCnt(post.getCommentCount())
			.writer(post.getUsers().getNickname())
			.date(post.getCreatedOn().toString())
			.build();
	}
}
