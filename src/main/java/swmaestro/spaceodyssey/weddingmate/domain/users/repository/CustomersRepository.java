package swmaestro.spaceodyssey.weddingmate.domain.users.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public interface CustomersRepository extends JpaRepository<Customers, Long> {
	Optional<Customers> findByUsers(Users users);

	@Modifying
	@Transactional
	@Query("UPDATE Customers c SET c.isDeleted = true WHERE c.customerId = :customerId")
	void softDeleteCustomerByCustomerId(@Param("customerId") Long customerId);
}
