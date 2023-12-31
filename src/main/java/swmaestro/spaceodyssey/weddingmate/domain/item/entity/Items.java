package swmaestro.spaceodyssey.weddingmate.domain.item.entity;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

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
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@Getter
@NoArgsConstructor
@Entity
@SQLDelete(sql = "UPDATE items SET is_deleted = true WHERE file_id = ?")
@Where(clause = "is_deleted = false")
public class Items extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long itemId;

	@NotNull(message = "아이템 태그는 빈칸일 수 없습니다")
	private String itemTagList;

	@Column(nullable = false)
	private String itemRecord;

	private String companyName;

	private String itemDate;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "portfolio_id")
	private Portfolios portfolios;

	private Integer itemOrder;

	@NotNull(message = "아이템 카테고리는 빈칸일 수 없습니다")
	private String category;

	@Column(nullable = false)
	private Boolean isDeleted = false;

	@OneToMany(mappedBy = "items", cascade = CascadeType.PERSIST)
	private List<Files> filesList = new ArrayList<>();

	private Integer likeCount = 0;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "company_id")
	private Companies companies;

	@Builder
	public Items(String itemRecord, String itemTagList, String itemDate, Integer itemOrder, Portfolios portfolios,
		String category) {
		this.itemRecord = itemRecord;
		this.itemTagList = itemTagList;
		this.itemDate = itemDate;
		this.itemOrder = itemOrder;
		this.portfolios = portfolios;
		this.category = category;
	}

	/*================== 기존 필드값 수정 ==================*/
	public void updateItem(ItemUpdateReqDto dto) {
		this.itemRecord = dto.getItemRecord();
		this.itemTagList = dto.getItemTagList();
		this.itemDate = dto.getDate();
		this.category = dto.getCategory();
	}

	public void updateOrder(Integer itemOrder) {
		this.itemOrder = itemOrder;
	}

	public void deleteItem() {
		this.isDeleted = true;
	}

	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	public void setCompanies(Companies companies) {
		this.companies = companies;
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