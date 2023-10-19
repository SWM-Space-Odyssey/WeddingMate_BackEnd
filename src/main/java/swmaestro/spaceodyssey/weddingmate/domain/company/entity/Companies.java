package swmaestro.spaceodyssey.weddingmate.domain.company.entity;

import java.util.ArrayList;
import java.util.List;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@Getter
@NoArgsConstructor
@Entity
@Schema(hidden = true)
public class Companies extends BaseTimeEntity {
	@OneToMany(mappedBy = "companies", cascade = CascadeType.PERSIST)
	private final List<Items> items = new ArrayList<>();
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long companyId;
	private String name;
	private String address;
	private String companyCategory;
	@Column(nullable = false)
	private Boolean isDeleted;
	@OneToOne(cascade = CascadeType.ALL)
	@JoinColumn(name = "file_id")
	private Files files;

	private Integer likeCount;

	@Builder
	public Companies(String name, String address, String category) {
		this.name = name;
		this.address = address;
		this.companyCategory = category;
		this.likeCount = 0;
	}

	public void increaseLikeCount() {
		this.likeCount += 1;
	}

	public void decreaseLikeCount() {
		validateLikeCount();
		this.likeCount -= 1;
	}

	private void validateLikeCount() {
		if (likeCount < 1) {
			throw new IllegalArgumentException();
		}
	}
}
