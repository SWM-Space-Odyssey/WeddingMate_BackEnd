package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface PlannersRepository extends JpaRepository<Planners, Long> {
	Optional<Planners> findByUsers(Users users);
	Optional<Planners> findByPlannerProfiles_PlannerProfileId(Long plannerProfileId);

	@Modifying
	@Transactional
	@Query("UPDATE Planners p SET p.isDeleted = true WHERE p.plannerId = :plannerId")
	void softDeletePlannerByPlannerId(@Param("plannerId") Long plannerId);
}
