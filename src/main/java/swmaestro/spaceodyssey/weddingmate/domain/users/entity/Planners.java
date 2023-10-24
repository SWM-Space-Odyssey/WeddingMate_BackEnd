package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import java.util.function.Consumer;

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
import jakarta.persistence.OneToOne;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfiles;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
@SQLDelete(sql = "UPDATE planners SET is_deleted = true WHERE planner_id = ?")
@Where(clause = "is_deleted = false")
public class Planners extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long plannerId;

	@OneToOne(mappedBy = "planners")
	@JoinColumn(name = "user_id")
	private Users users;

	@NotNull(message = "소속은 필수로 입력되어야 합니다.")
	private String company;

	@NotNull(message = "직급은 필수로 입력되어야 합니다.")
	private String position;

	@NotNull(message = "지역은 필수로 입력되어야 합니다.")
	private String regionList; // 최대 3개

	@OneToOne(cascade = CascadeType.PERSIST, fetch = FetchType.EAGER)
	@JoinColumn(name = "profile_id")
	private PlannerProfiles plannerProfiles;

	private String plannerTagList; // 최대 3개

	private Integer likeCount = 0;

	@Column(nullable = false)
	private Boolean isDeleted = false;

	@Builder
	public Planners(String company, String position, String regionList, String plannerTagList) {
		this.company = company;
		this.position = position;
		this.regionList = regionList;
		this.plannerTagList = plannerTagList;
	}

	public void updatePlannerInfo(PlannerInfoDto dto) {
		updateFieldIfNotNull(dto.getCompany(), this::updateCompany);
		updateFieldIfNotNull(dto.getPosition(), this::updatePosition);
		updateFieldIfNotNull(dto.getRegionList(), this::updateRegionList);
		updateFieldIfNotNull(dto.getTagList(), this::updatePlannerTagList);
	}

	/*================== 초기값 설정 ===================*/
	public void setUsers(Users users) {
		this.users = users;
		users.setPlanners(this);
	}

	public void setPlannerProfiles(PlannerProfiles plannerProfiles) {
		this.plannerProfiles = plannerProfiles;
		plannerProfiles.setPlanners(this);
	}

	/*================== 기존 필드값 수정 ==================*/
	public void updateCompany(String company) {
		this.company = company;
	}

	public void updatePosition(String position) {
		this.position = position;
	}

	public void updateRegionList(String regionList) {
		this.regionList = regionList;
	}

	public void updatePlannerTagList(String plannerTagList) {
		this.plannerTagList = plannerTagList;
	}

	private void updateFieldIfNotNull(String newValue, Consumer<String> fieldUpdater) {
		if (newValue != null) {
			fieldUpdater.accept(newValue);
		}
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

	public void deletePlanners() {
		this.isDeleted = true;
	}
}