package swmaestro.spaceodyssey.weddingmate.domain.tag.entity;

import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.entity.Category;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.ItemTag;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.PortfolioTag;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor
@Getter
@Entity
public class Tag extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long tagId;

	@Column(nullable = false)
	private String content;

	@Column(nullable = false)
	private Boolean isDefault;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "category_id")
	private Category category;

	@OneToMany(mappedBy = "tag")
	private List<ItemTag> itemTagList;

	@OneToMany(mappedBy = "tag")
	private List<PortfolioTag> portfolioTagList;

	@Builder
	public Tag(String content, Category category, Boolean isDefault) {
		this.content = content;
		this.category = category;
		this.isDefault = isDefault;
	}
}
