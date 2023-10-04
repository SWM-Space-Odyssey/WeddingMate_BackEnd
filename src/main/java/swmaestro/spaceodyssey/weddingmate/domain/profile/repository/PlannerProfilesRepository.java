package swmaestro.spaceodyssey.weddingmate.domain.profile.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfiles;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;

public interface PlannerProfilesRepository extends JpaRepository<PlannerProfiles, Long> {
	Optional<PlannerProfiles> findPlannerProfilesByPlanners(Planners planners);

	Optional<PlannerProfiles> findById(Long plannerProfileId);
}
