package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

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
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerTagListDto;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
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
	}

	public void setUsers(Users users) {
		this.users = users;
		users.setCustomers(this);
	}

	public void setWeddingDate(String weddingDate) {
		this.weddingDate = weddingDate;
	}
}
