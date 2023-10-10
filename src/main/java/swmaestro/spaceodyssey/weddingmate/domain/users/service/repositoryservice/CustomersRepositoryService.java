package swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.CustomersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.CustomerNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class CustomersRepositoryService {

	private final CustomersRepository customersRepository;

	@Transactional
	public Customers findCustomerByUser(Users users) {
		return customersRepository.findByUsers(users)
			.orElseThrow(CustomerNotFoundException::new);
	}

	@Transactional
	public void softDeleteCustomerByCustomerId(Long customerId) {
		customersRepository.softDeleteCustomerByCustomerId(customerId);
	}
}
