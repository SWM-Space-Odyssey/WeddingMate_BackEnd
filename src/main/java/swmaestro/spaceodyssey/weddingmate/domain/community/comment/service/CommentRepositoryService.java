package swmaestro.spaceodyssey.weddingmate.domain.community.comment.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.netty.handler.codec.marshalling.CompatibleMarshallingEncoder;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.entity.Comments;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.repository.CommentsRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.community.comment.CommentNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class CommentRepositoryService {
	private final CommentsRepository commentsRepository;

	public Comments findById(Long id) {
		return commentsRepository.findById(id)
			.orElseThrow(CommentNotFoundException::new);
	}
}
