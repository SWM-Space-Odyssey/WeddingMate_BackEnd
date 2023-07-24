package swmaestro.spaceodyssey.weddingmate.domain.item.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;

@Entity
@Getter
@NoArgsConstructor
public class ItemTag {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@ManyToOne
	@JoinColumn
	private Item item;

	@ManyToOne
	@JoinColumn
	private Tag tag;

	@Builder
	public ItemTag(Item item, Tag tag) {
		this.item = item;
		this.tag = tag;
	}
}
