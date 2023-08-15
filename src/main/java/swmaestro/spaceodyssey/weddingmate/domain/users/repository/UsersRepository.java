package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;

public interface UsersRepository extends JpaRepository<Users, Long> {
	Optional<Users> findByEmail(String email);

	Optional<Users> findByPlanner(Planner planner);

	Optional<Users> findByAuthProviderId(String authProviderId);

	List<Users> findAllByRegisterStatus(UserRegisterStatusEnum registerStatusEnum);
}
