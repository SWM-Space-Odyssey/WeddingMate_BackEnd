package swmaestro.spaceodyssey.weddingmate.domain.file.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@Entity
@Getter
@NoArgsConstructor
public class File extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long fileId;

	@Column(nullable = false)
	private String filename;

	@Column(nullable = false)
	private String filetype;

	@Column(nullable = false)
	private String url;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "item_id")
	private Item item;

	public void setItem(Item item) {
		this.item = item;
	}

	@Builder
	public File(String filename, String filetype, String url) {
		this.filename = filename;
		this.filetype = filetype;
		this.url = url;
	}
}

