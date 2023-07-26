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
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
public class Planner extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long plannerId;

	@OneToOne(mappedBy = "planner")
	private Users users;

	@NotNull(message = "소속은 필수로 입력되어야 합니다.")
	private String company;

	@NotNull(message = "직급은 필수로 입력되어야 합니다.")
	private String position;

	@NotNull(message = "지역은 필수로 입력되어야 합니다.")
	private String region;

	@OneToMany(mappedBy = "planner", cascade = CascadeType.PERSIST)
	private List<PlannerTag> plannerTagList = new ArrayList<>();

	@Builder
	public Planner(String company, String position, String region) {
		this.company = company;
		this.position = position;
		this.region = region;
	}

	public void setUsers(Users users) {
		this.users = users;
		users.setPlanner(this);
	}
	public void createPlannerTagList(List<PlannerTag> plannerTagList) {
		this.plannerTagList = plannerTagList;
	}
}
