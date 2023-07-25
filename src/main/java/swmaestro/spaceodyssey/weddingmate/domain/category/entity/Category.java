package swmaestro.spaceodyssey.weddingmate.domain.category.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Getter
@Entity
public class Category {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long categoryId;

	@Column(nullable = false)
	private String content;

	@Column(nullable = false)
	private Boolean isDefault;

	@Builder
	public Category(String content, Boolean isDefault) {
		this.content = content;
		this.isDefault = isDefault;
	}
}
