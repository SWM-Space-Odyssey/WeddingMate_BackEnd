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
import jakarta.validation.constraints.NotNull;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@Getter
@NoArgsConstructor
@Entity
public class Items extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long itemId;

	@NotNull(message = "아이템 태그는 빈칸일 수 없습니다")
	private String itemTagList;

	@Column(nullable = false)
	private String itemRecord;

	private String company;

	private String itemDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "portfolio_id")
	private Portfolios portfolios;

	private Integer itemOrder;

	@NotNull(message = "아이템 카테고리는 빈칸일 수 없습니다")
	private String category;

	@Column(nullable = false)
	private Boolean isDeleted;

	@OneToMany(mappedBy = "items", cascade = CascadeType.PERSIST)
	private List<Files> filesList = new ArrayList<>();

	private Integer likeCount;


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
	public Items(String itemRecord, String itemTagList, String company, String itemDate, Integer itemOrder, Portfolios portfolios, String category) {
		this.itemRecord = itemRecord;
		this.itemTagList = itemTagList;
		this.company = company;
		this.itemDate = itemDate;
		this.itemOrder = itemOrder;
		this.portfolios = portfolios;
		this.category = category;
		this.isDeleted = false;
		this.likeCount = 0;
	}

	public void setLikeCount(Integer likeCount) {
		this.likeCount = likeCount;
	}

}