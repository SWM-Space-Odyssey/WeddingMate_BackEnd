package swmaestro.spaceodyssey.weddingmate.domain.community.comment.mapper;

import org.springframework.stereotype.Component;

import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.dto.CommentResDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.dto.CommentSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.entity.Comments;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.entity.Posts;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Component
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class CommentMapper {

	public Comments dtoToEntity(Users user, Posts post, CommentSaveReqDto dto) {

		return Comments.builder()
			.content(dto.getContent())
			.posts(post)
			.users(user)
			.build();
	}

	public CommentResDto entityToDto(Comments comment) {

		return CommentResDto.builder()
			.id(comment.getCommentId())
			.content(comment.getContent())
			.userId(comment.getUsers().getUserId())
			.date(comment.getCreatedOn().toString())
			.writer(comment.getUsers().getNickname())
			.profileImg(comment.getUsers().getProfileImage().getUrl())
			.build();
	}
}
