package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.CustomerInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerTagListDto;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
@SQLDelete(sql = "UPDATE customers SET is_deleted = true WHERE customer_id = ?")
@Where(clause = "is_deleted = false")
public class Customers extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long customerId;

	@OneToOne(mappedBy = "customers")
	private Users users;

	@NotNull(message = "예식일 확정 여부는 필수로 입력되어야 합니다.")
	private Boolean weddingDateConfirmed;

	private String weddingDate;

	private String regionList; // 최대 2개

	@NotNull(message = "예산은 필수로 입력되어야 합니다.")
	private String budget;

	private String portfolioTagList;

	private String plannerTagList;

	private String dressTagList;

	private String studioTypeTagList;

	private String studioFocusTagList;

	private String makeupTagList;

	@Column(nullable = false)
	private Boolean isDeleted;


	@Builder
	public Customers(Boolean weddingDateConfirmed, String regionList, String budget, CustomerTagListDto customerTagList) {
		this.weddingDateConfirmed = weddingDateConfirmed;
		this.regionList = regionList;
		this.budget = budget;
		this.portfolioTagList = customerTagList.getPortfolioTagList();
		this.plannerTagList = customerTagList.getPlannerTagList();
		this.dressTagList = customerTagList.getDressTagList();
		this.studioTypeTagList = customerTagList.getStudioTypeTagList();
		this.studioFocusTagList = customerTagList.getStudioFocusTagList();
		this.makeupTagList = customerTagList.getMakeupTagList();
		this.isDeleted = false;
	}

	public void updateCustomerInfo(CustomerInfoDto dto) {
		this.weddingDate = dto.getWeddingDate();
		this.weddingDateConfirmed = dto.getWeddingDateConfirmed();
		this.regionList = dto.getRegionList();
		this.budget = dto.getBudget();
	}

	public void updateCustomerTagList(CustomerTagListDto dto) {
		this.portfolioTagList = dto.getPortfolioTagList();
		this.plannerTagList = dto.getPlannerTagList();
		this.dressTagList = dto.getDressTagList();
		this.studioTypeTagList = dto.getStudioTypeTagList();
		this.studioFocusTagList = dto.getStudioFocusTagList();
		this.makeupTagList = dto.getMakeupTagList();
	}

	/*================== 초기값 설정 ===================*/
	public void setUsers(Users users) {
		this.users = users;
		users.setCustomers(this);
	}

	/*================== 기존 필드값 수정 ==================*/
	public void setWeddingDate(String weddingDate) {
		this.weddingDate = weddingDate;
	}

	public void deleteCustomers() {
		this.isDeleted = true;
	}
}
