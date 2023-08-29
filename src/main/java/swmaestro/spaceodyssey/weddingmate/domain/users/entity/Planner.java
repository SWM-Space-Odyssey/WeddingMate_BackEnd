package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import java.util.List;
import java.util.function.Consumer;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerInfoDto;
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

	@OneToOne(cascade = CascadeType.PERSIST, fetch = FetchType.EAGER)
	@JoinColumn(name = "profile_id")
	private PlannerProfile plannerProfile;

	@OneToMany(mappedBy = "planner", cascade = CascadeType.ALL, orphanRemoval = true)
	private List<Portfolio> portfolioList;

	private String plannerTagList;

	private Integer likeCount;

	@Builder
	public Planner(String company, String position, String region, String plannerTagList) {
		this.company = company;
		this.position = position;
		this.region = region;
		this.plannerTagList = plannerTagList;
	}

	public void updatePlannerInfo(PlannerInfoDto dto) {
		updateFieldIfNotNull(dto.getCompany(), this::updateCompany);
		updateFieldIfNotNull(dto.getPosition(), this::updatePosition);
		updateFieldIfNotNull(dto.getRegion(), this::updateRegion);
		updateFieldIfNotNull(dto.getTagList(), this::updatePlannerTagList);
	}

	/*================== 초기값 설정 ===================*/
	public void setUsers(Users users) {
		this.users = users;
		users.setPlanner(this);
	}

	public void setPlannerProfile(PlannerProfile plannerProfile) {
		this.plannerProfile = plannerProfile;
		plannerProfile.setPlanner(this);
	}

	/*================== 기존 필드값 수정 ==================*/
	public void updateCompany(String company) {
		this.company = company;
	}

	public void updatePosition(String position){
		this.position = position;
	}

	public void updateRegion(String region){
		this.region = region;
	}

	public void updatePlannerTagList(String plannerTagList){
		this.plannerTagList = plannerTagList;
	}

	private void updateFieldIfNotNull(String newValue, Consumer<String> fieldUpdater) {
		if (newValue != null) {
			fieldUpdater.accept(newValue);
		}
	}

	public void setLikeCount(Integer likeCount) {
		this.likeCount = likeCount;
	}
}