package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;

public interface CustomersRepository extends JpaRepository<Customers, Long> {
}
