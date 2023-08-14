package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface PlannerRepository extends JpaRepository<Planner, Long> {
	Optional<Planner> findByUsers(Users users);
	Optional<Planner> findByPlannerProfile_PlannerProfileId(Long plannerProfileId);
}
