package swmaestro.spaceodyssey.weddingmate.domain.file.entity;

import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

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
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@Entity
@Getter
@NoArgsConstructor
@SQLDelete(sql = "UPDATE files SET is_deleted = true WHERE file_id = ?")
@Where(clause = "is_deleted = false")
public class Files extends BaseTimeEntity {

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
	private Items items;

	@Column(nullable = false)
	private Boolean isDeleted = false;

	@Builder
	public Files(String filename, String filetype, String url) {
		this.filename = filename;
		this.filetype = filetype;
		this.url = url;
	}

	public void setItems(Items items) {
		this.items = items;
	}

	public void deleteFile() {
		this.isDeleted = true;
	}
}

