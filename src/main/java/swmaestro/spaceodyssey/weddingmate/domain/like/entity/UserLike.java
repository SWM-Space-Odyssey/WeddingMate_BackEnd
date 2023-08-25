package swmaestro.spaceodyssey.weddingmate.domain.like.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Entity
@Getter
@NoArgsConstructor
public class UserLike {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id")
	private Users users;

	@Enumerated(EnumType.STRING) // Enum 타입으로 저장되도록 설정
	@Column(nullable = false)
	private LikeEnum likeType; // Enum 타입으로 변경

	@Column(nullable = false)
	private Long likedId; // 해당 유형의 테이블의 주 키와 연결

	@Builder
	public UserLike(Users users, LikeEnum likeEnum, Long likedId) {
		this.users = users;
		this.likeType = likeEnum;
		this.likedId = likedId;
	}
}
