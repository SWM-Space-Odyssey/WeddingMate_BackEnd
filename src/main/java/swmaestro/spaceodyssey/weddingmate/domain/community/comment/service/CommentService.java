package swmaestro.spaceodyssey.weddingmate.domain.community.comment.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;

import swmaestro.spaceodyssey.weddingmate.domain.community.comment.dto.CommentSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.entity.Comments;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.entity.Posts;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.mapper.CommentMapper;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.repository.CommentsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.service.PostRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

@Service
@RequiredArgsConstructor
@Transactional
public class CommentService {

	private final PostRepositoryService postRepositoryService;
	private final CommentRepositoryService commentRepositoryService;
	private final CommentsRepository commentsRepository;
	private final CommentMapper commentMapper;

	public Long createComment(Users user, Long postId, CommentSaveReqDto commentSaveReqDto) {
		Posts post = postRepositoryService.findPostById(postId);

		Comments comment = commentMapper.dtoToEntity(user, post, commentSaveReqDto);

		commentsRepository.save(comment);

		post.addCntComment();

		return comment.getCommentId();
	}

	public void deleteComment(Users user, Long id) {
		Comments comment = commentRepositoryService.findById(id);

		Posts post = postRepositoryService.findPostById(comment.getPosts().getPostId());

		verifyUserIsWriter(comment, user);

		post.deleteCntComment();

		comment.deleteComment();
	}

	/*================== 예외 처리 ==================*/
	public void verifyUserIsWriter(Comments comment, Users users) {
		if (!comment.getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}
	}

	public Boolean checkUserIsWriter(Comments comment, Users users) {
		return comment.getUsers().getUserId().equals(users.getUserId());
	}
}
