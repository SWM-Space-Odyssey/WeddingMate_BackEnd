package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.SuspensionPeriodEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserAccountStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.global.config.aop.DistributedLock.DistributedLock;
import swmaestro.spaceodyssey.weddingmate.global.constant.UserConstant;

import java.time.LocalDateTime;

@RequiredArgsConstructor
@Service
public class UserSuspensionService {

	private final UsersRepositoryService usersRepositoryService;

	@DistributedLock(key = "#lockName")
	public void addReportCnt(String lockName, Long userId) {
		System.out.println("addReportCnt CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		Users pUser = usersRepositoryService.findUserById(userId);
		pUser.incrementReportCnt();
		checkReportCnt(pUser.getUserId());
	}

	public void checkReportCnt(Long userId) {
		System.out.println("CheckReportCnt CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		Users pUser = usersRepositoryService.findUserById(userId);
		if (pUser.getReportCnt() == UserConstant.REPORT_LIMIT) {
			pUser.resetReportCnt();
			blockUser(pUser);
		}
	}

	public void blockUser(Users user) {
		System.out.println("blockUser CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		user.setAccountStatusToSuspended();
		user.incrementBlockCnt();
		checkBlockedCnt(user);

		if (!user.getAccountStatus().equals(UserAccountStatusEnum.BANNED)) {
			setSuspensionPeriodByBlockCnt(user);
		}
	}

	public void setSuspensionPeriodByBlockCnt(Users user) {
		System.out.println("setSuspensionPeriodByBlockCnt CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		SuspensionPeriodEnum period = SuspensionPeriodEnum.getByBlockCnt(user.getBlockCnt());
		if (period != null) {
			LocalDateTime newSuspensionEndTime = LocalDateTime.now().plusDays(period.getDays());
			user.setSuspensionEndTime(newSuspensionEndTime);
		}
	}

	public void checkBlockedCnt(Users user) {
		System.out.println("checkBlockedCnt CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		if (user.getBlockCnt().equals(UserConstant.BLOCKED_LIMIT)) {
			banUser(user);
		}
	}

	public void banUser(Users user) {
		System.out.println("banUser CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		user.setAccountStatusToBanned();
	}
}
