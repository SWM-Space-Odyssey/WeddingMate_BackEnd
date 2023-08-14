package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;
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

	@OneToOne(cascade = CascadeType.PERSIST, fetch = FetchType.LAZY)
	@JoinColumn(name = "profile_id")
	private PlannerProfile plannerProfile;

	private String plannerTagList;

	@Builder
	public Planner(String company, String position, String region, String plannerTagList) {
		this.company = company;
		this.position = position;
		this.region = region;
		this.plannerTagList = plannerTagList;
	}

	public void setUsers(Users users) {
		this.users = users;
		users.setPlanner(this);
	}

	public void setPlannerProfile(PlannerProfile plannerProfile) {
		this.plannerProfile = plannerProfile;
		plannerProfile.setPlanner(this);
	}
}