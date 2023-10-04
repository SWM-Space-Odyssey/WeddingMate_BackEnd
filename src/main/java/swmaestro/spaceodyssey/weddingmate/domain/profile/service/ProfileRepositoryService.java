package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.profile.entity.PlannerProfiles;
import swmaestro.spaceodyssey.weddingmate.domain.profile.repository.PlannerProfilesRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.profile.PlannerProfileNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class ProfileRepositoryService {

	private final PlannerProfilesRepository plannerProfilesRepository;

	@Transactional
	public PlannerProfiles findPlannerProfileById(Long plannerProfileId) {
		return plannerProfilesRepository.findById(plannerProfileId)
			.orElseThrow(PlannerProfileNotFoundException::new);
	}
}
