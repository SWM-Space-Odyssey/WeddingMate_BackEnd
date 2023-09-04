package swmaestro.spaceodyssey.weddingmate.domain.profile.entity;

import java.util.function.Consumer;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.dto.PlannerProfileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
public class PlannerProfiles extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long plannerProfileId;

	@OneToOne(mappedBy = "plannerProfiles")
	private Planners planners;

	private String bio;

	private String sns;

	@Builder
	public PlannerProfiles(String bio, String sns) {
		this.bio = bio;
		this.sns = sns;
	}

	public void updatePlannerProfileInfo(PlannerProfileInfoDto dto) {
		updateFieldIfNotNull(dto.getBio(), this::updateBio);
		updateFieldIfNotNull(dto.getSns(), this::updateSns);
	}

	/*================== 초기값 설정 ===================*/
	public void setPlanners(Planners planners) {
		this.planners = planners;

		if (planners.getPlannerProfiles() != this) {
			planners.setPlannerProfiles(this);
		}
	}

	/*================== 기존 필드값 수정 ==================*/
	public void updateBio(String bio){
		this.bio = bio;
	}

	public void updateSns(String sns){
		this.sns = sns;
	}

	private void updateFieldIfNotNull(String newValue, Consumer<String> fieldUpdater) {
		if (newValue != null) {
			fieldUpdater.accept(newValue);
		}
	}
}
