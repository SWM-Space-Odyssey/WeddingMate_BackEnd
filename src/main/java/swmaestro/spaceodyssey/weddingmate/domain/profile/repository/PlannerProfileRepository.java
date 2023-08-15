package swmaestro.spaceodyssey.weddingmate.domain.profile.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfile;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;

public interface PlannerProfileRepository extends JpaRepository<PlannerProfile, Long> {
	Optional<PlannerProfile> findPlannerProfileByPlanner(Planner planner);
}
