package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.PortfolioTag;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
public class Customer extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long customerId;

	@OneToOne(mappedBy = "customer")
	private Users users;

	private String weddingDate;

	@NotNull(message = "예산은 필수로 입력되어야 합니다.")
	private String budget;

	@OneToMany(mappedBy = "customer", cascade = CascadeType.PERSIST)
	private List<PortfolioTag> portfolioTagList = new ArrayList<>();

	@OneToMany(mappedBy = "customer", cascade = CascadeType.PERSIST)
	private List<PlannerTag> plannerTagList = new ArrayList<>();

	@Builder
	public Customer(String weddingDate, String budget) {
		this.weddingDate = weddingDate;
		this.budget = budget;
	}

	public void setUsers(Users users) {
		this.users = users;
		users.setCustomer(this);
	}

	public void createPortfolioTagList(List<PortfolioTag> portfolioTagList) {
		this.portfolioTagList = portfolioTagList;
	}

	public void createPlannerTagList(List<PlannerTag> plannerTagList) {
		this.plannerTagList = plannerTagList;
	}
}
