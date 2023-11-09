package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.SuspensionPeriodEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserAccountStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;
import swmaestro.spaceodyssey.weddingmate.global.config.aop.distributed_lock.DistributedLock;
import swmaestro.spaceodyssey.weddingmate.global.constant.UserConstant;

import java.time.LocalDateTime;

@Slf4j
@RequiredArgsConstructor
@Service
public class UserSuspensionService {

	private final UsersRepositoryService usersRepositoryService;

	@DistributedLock(key = "#lockName")
	public void addReportCnt(String lockName, Long userId) {
		log.info("addReportCnt CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		Users pUser = usersRepositoryService.findUserById(userId);
		pUser.incrementReportCnt();
		checkReportCnt(pUser.getUserId());
	}

	public void checkReportCnt(Long userId) {
		log.info("checkReportCnt CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		Users pUser = usersRepositoryService.findUserById(userId);
		if (pUser.getReportCnt() == UserConstant.REPORT_LIMIT) {
			pUser.resetReportCnt();
			blockUser(pUser);
		}
	}

	public void blockUser(Users user) {
		log.info("blockUser CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		user.setAccountStatusToSuspended();
		user.incrementBlockCnt();
		checkBlockedCnt(user);

		if (!user.getAccountStatus().equals(UserAccountStatusEnum.BANNED)) {
			setSuspensionPeriodByBlockCnt(user);
		}
	}

	public void setSuspensionPeriodByBlockCnt(Users user) {
		log.info("setSuspensionPeriodByBlockCnt CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		SuspensionPeriodEnum period = SuspensionPeriodEnum.getByBlockCnt(user.getBlockCnt());
		if (period != null) {
			LocalDateTime newSuspensionEndTime = LocalDateTime.now().plusDays(period.getDays());
			user.setSuspensionEndTime(newSuspensionEndTime);
		}
	}

	public void checkBlockedCnt(Users user) {
		log.info("checkBlockedCnt CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		if (user.getBlockCnt().equals(UserConstant.BLOCKED_LIMIT)) {
			banUser(user);
		}
	}

	public void banUser(Users user) {
		log.info("banUser CurrentTransactionName: " + TransactionSynchronizationManager.getCurrentTransactionName());
		user.setAccountStatusToBanned();
	}
}
