package swmaestro.spaceodyssey.weddingmate.domain.company.entity;

import java.util.ArrayList;
import java.util.List;

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
public class Companies extends BaseTimeEntity {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long companyId;

	private String name;

	private String address;

	private String companyCategory;

	@Column(nullable = false)
	private Boolean isDeleted;

	@OneToMany(mappedBy = "companies", cascade = CascadeType.PERSIST)
	private final List<Items> items = new ArrayList<>();

	@OneToOne(cascade = CascadeType.ALL)
	@JoinColumn(name = "file_id")
	private Files files;

	@Builder
	public Companies(String name, String address, String category) {
		this.name = name;
		this.address = address;
		this.companyCategory = category;
	}
}
