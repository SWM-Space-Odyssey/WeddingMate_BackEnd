package swmaestro.spaceodyssey.weddingmate.domain.item.entity;

import java.util.ArrayList;
import java.util.List;

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
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@Getter
@NoArgsConstructor
@Entity
public class Item extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long itemId;

	private String itemTagList;

	@Column(nullable = false)
	private String itemRecord;

	@Column(nullable = false)
	private String company;

	private String itemDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "portfolio_id")
	private Portfolio portfolio;

	private Integer itemOrder;

	private String category;

	@Column(nullable = false)
	private Boolean isDeleted;

	@OneToMany(mappedBy = "item", cascade = CascadeType.PERSIST)
	private List<File> fileList = new ArrayList<>();


	public void updateItem(String itemRecord, String itemTagList, String company, String itemDate, String category) {
		this.itemRecord = itemRecord;
		this.itemTagList = itemTagList;
		this.company = company;
		this.itemDate = itemDate;
		this.category = category;
	}

	public void updateOrder(Integer itemOrder) {
		this.itemOrder = itemOrder;
	}

	public void deleteItem() {
		this.isDeleted = true;
	}
	@Builder
	public Item(String itemRecord, String itemTagList, String company, String itemDate, Integer itemOrder, Portfolio portfolio, String category) {
		this.itemRecord = itemRecord;
		this.itemTagList = itemTagList;
		this.company = company;
		this.itemDate = itemDate;
		this.itemOrder = itemOrder;
		this.portfolio = portfolio;
		this.category = category;
		this.isDeleted = false;
	}
}
