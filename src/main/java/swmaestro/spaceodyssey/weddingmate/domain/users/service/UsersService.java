package swmaestro.spaceodyssey.weddingmate.domain.users.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.service.ItemsRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.like.service.LikesRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.service.PortfolioRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserAccountStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.CustomersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.PlannersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.CustomerDuplicateRegistrationException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.PlannerDuplicateRegistrationException;

@Transactional
@Service
@RequiredArgsConstructor
public class UsersService {

	private final UsersRepositoryService usersRepositoryService;

	private final PlannersRepositoryService plannersRepositoryService;
	private final CustomersRepositoryService customersRepositoryService;
	private final PortfolioRepositoryService portfolioRepositoryService;
	private final ItemsRepositoryService itemsRepositoryService;
	private final LikesRepositoryService likesRepositoryService;

	@Transactional
	public UserRegisterStatusEnum getUserRegisterStatus(Users users) {
		Users pUsers = usersRepositoryService.findUserByEmail(users.getEmail());
		return pUsers.getRegisterStatus();
	}

	@Transactional
	public UserAccountStatusEnum checkUsersAccountStatus(Users users) {
		Users pUsers = usersRepositoryService.findUserByEmail(users.getEmail());
		return pUsers.getAccountStatus();
	}

	@Transactional
	public void softDeleteUserRelatedEntity(Users users) {
		softDeletePlannerOrCustomer(users);
		portfolioRepositoryService.softDeletePortfolioByUsers(users);
		itemsRepositoryService.softDeleteItemsByUserId(users.getUserId());
		likesRepositoryService.softDeleteLikesByUsers(users);
	}

	@Transactional
	public void softDeletePlannerOrCustomer(Users users) {
		if (users.getRegisterStatus() == UserRegisterStatusEnum.PLANNER)
			plannersRepositoryService.softDeletePlannerByPlannerId(users.getPlanners().getPlannerId());
		else
			customersRepositoryService.softDeleteCustomerByCustomerId(users.getCustomers().getCustomerId());
	}

	/*================== 예외 처리 ==================*/
	@Transactional(readOnly = true)
	public void checkUserIsRegistered(Users users) {
		UserRegisterStatusEnum status = users.getRegisterStatus();
		if (status == UserRegisterStatusEnum.PLANNER) {
			throw new PlannerDuplicateRegistrationException();
		} else if (status == UserRegisterStatusEnum.CUSTOMER) {
			throw new CustomerDuplicateRegistrationException();
		}
	}
}