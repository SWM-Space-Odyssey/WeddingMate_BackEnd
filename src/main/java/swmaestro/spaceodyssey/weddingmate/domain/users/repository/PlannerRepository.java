package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;

public interface PlannerRepository extends JpaRepository<Planner, Long> {
	Optional<Planner> findByUsers(Users users);
}
