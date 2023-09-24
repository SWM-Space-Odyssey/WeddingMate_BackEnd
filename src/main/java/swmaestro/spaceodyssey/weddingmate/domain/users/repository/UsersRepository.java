package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;

public interface UsersRepository extends JpaRepository<Users, Long> {
	Optional<Users> findByEmail(String email);

	Optional<Users> findByPlanners(Planners planners);

	Optional<Users> findByCustomers(Customers customers);

	Optional<Users> findByAuthProviderId(String authProviderId);

	List<Users> findAllByRegisterStatus(UserRegisterStatusEnum registerStatusEnum);

	Optional<Users> findByNickname(String nickname);
}
