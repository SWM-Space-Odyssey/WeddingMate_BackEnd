package swmaestro.spaceodyssey.weddingmate.domain.community.post.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.community.post.entity.Posts;

public interface PostsRepository extends JpaRepository<Posts, Long> {
}
