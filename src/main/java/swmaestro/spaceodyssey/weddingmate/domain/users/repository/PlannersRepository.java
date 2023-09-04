package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface PlannersRepository extends JpaRepository<Planners, Long> {
	Optional<Planners> findByUsers(Users users);
	Optional<Planners> findByPlannerProfiles_PlannerProfileId(Long plannerProfileId);
}
