package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface CustomersRepository extends JpaRepository<Customers, Long> {
	Optional<Customers> findByUsers(Users users);
}
