package swmaestro.spaceodyssey.weddingmate.domain.community.post.entity;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.entity.Comments;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@SQLDelete(sql = "UPDATE posts SET is_deleted = true WHERE post_id = ?")
@Where(clause = "is_deleted = false")
@Entity
public class Posts extends BaseTimeEntity {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long postId;

	@Column(length =  50, nullable = false)
	private String title;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id")
	private Users users;

	@Column(nullable = false)
	private Boolean isDeleted = false;

	@OneToMany(mappedBy = "posts", cascade = CascadeType.PERSIST, orphanRemoval = true)
	private List<Comments> comments = new ArrayList<>();

	private Integer viewCount = 0;

	private Integer commentCount = 0;

	@NotNull(message = "카테고리는 빈칸일 수 없습니다")
	private String category;

	@Column(length =  500, nullable = false)
	private String content;

	@Builder
	public Posts(String title, Users user, String category, String content) {
		this.title = title;
		this.category = category;
		this.content = content;
		this.users = user;
	}

	public void updatePost(String title, String category, String content) {
		this.title = title;
		this.category = category;
		this.content = content;
	}

	public void addCntComment() {
		this.commentCount += 1;
	}

	public void deleteCntComment() {
		validateCntComment();
		this.commentCount -= 1;
	}

	private void validateCntComment() {
		if (commentCount < 1) {
			throw new IllegalArgumentException();
		}
	}

	public void addViewCount() {
		this.viewCount += 1;
	}

	public void deletePost() {
		this.isDeleted = true;
	}
}
