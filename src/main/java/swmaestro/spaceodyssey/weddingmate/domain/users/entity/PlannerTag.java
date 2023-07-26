package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;

@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
public class PlannerTag {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long plannerTagId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn
	private Planner planner;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn
	private Customer customer;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn
	private Tag tag;

	@Builder(builderMethodName = "plannerBuilder")
	public PlannerTag(Planner planner, Tag tag) {
		this.planner = planner;
		this.tag = tag;
	}

	@Builder(builderMethodName = "customerBuilder")
	public PlannerTag(Customer customer, Tag tag) {
		this.customer = customer;
		this.tag = tag;
	}
}
