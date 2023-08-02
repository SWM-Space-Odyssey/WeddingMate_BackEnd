package swmaestro.spaceodyssey.weddingmate.domain.profile.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
public class PlannerProfile extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long plannerProfileId;

	@OneToOne(mappedBy = "plannerProfile")
	private Planner planner;

	private String bio;

	private String sns;

	@Builder
	public PlannerProfile(String bio, String sns) {
		this.bio = bio;
		this.sns = sns;
	}

	public void setPlanner(Planner planner) {
		this.planner = planner;

		if (planner.getPlannerProfile() != this) {
			planner.setPlannerProfile(this);
		}
	}

	public void updateBio(String bio){
		this.bio = bio;
	}

	public void updateSns(String sns){
		this.sns = sns;
	}
}
