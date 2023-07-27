package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;

public interface CustomerRepository extends JpaRepository<Customer, Long> {
}
