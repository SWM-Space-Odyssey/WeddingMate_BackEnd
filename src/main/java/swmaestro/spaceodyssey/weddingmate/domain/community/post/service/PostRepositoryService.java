package swmaestro.spaceodyssey.weddingmate.domain.community.post.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.entity.Posts;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.repository.PostsRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.community.post.PostNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class PostRepositoryService {
	private final PostsRepository postsRepository;

	public Posts findPostById(Long id) {
		return postsRepository.findById(id)
			.orElseThrow(PostNotFoundException::new);
	}
}
