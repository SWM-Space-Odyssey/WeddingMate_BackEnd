package swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.PlannersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class PlannersRepositoryService {

	private final PlannersRepository plannersRepository;

	@Transactional
	public Planners findPlannerById(Long id) {
		return plannersRepository.findById(id)
			.orElseThrow(PlannerNotFoundException::new);
	}

	@Transactional
	public Planners findPlannerByPlannerProfileId(Long plannerProfileId) {
		return plannersRepository.findByPlannerProfiles_PlannerProfileId(plannerProfileId)
			.orElseThrow(PlannerNotFoundException::new);
	}

	@Transactional
	public Planners findPlannerByUsers(Users users) {
		return plannersRepository.findByUsers(users)
			.orElseThrow(PlannerNotFoundException::new);
	}

	@Transactional
	public void softDeletePlannerByPlannerId(Long plannerId) {
		plannersRepository.softDeletePlannerByPlannerId(plannerId);
	}
}
