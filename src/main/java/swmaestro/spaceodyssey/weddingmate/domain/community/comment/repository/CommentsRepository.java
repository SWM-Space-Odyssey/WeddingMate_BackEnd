package swmaestro.spaceodyssey.weddingmate.domain.community.comment.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.community.comment.entity.Comments;

public interface CommentsRepository extends JpaRepository<Comments, Long> {
}
