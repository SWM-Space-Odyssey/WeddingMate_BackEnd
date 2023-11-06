package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
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
	public Users addReportCnt(String lockName, Long userId) {
		Users pUsers = usersRepositoryService.findUserById(userId);
		pUsers.incrementReportCnt();
		checkReportCnt(pUsers);
		return pUsers;
	}

	//@Transactional
	public void checkReportCnt(Users user) {
		if (user.getReportCnt() == UserConstant.REPORT_LIMIT) {
			user.resetReportCnt();
			blockUser(user);
		}
	}

	//@Transactional
	public void blockUser(Users user) {
		user.setAccountStatusToSuspended();
		user.incrementBlockCnt();
		checkBlockedCnt(user);

		if (!user.getAccountStatus().equals(UserAccountStatusEnum.BANNED)) {
			setSuspensionPeriodByBlockCnt(user);
		}
	}

	//	@Transactional
	public void setSuspensionPeriodByBlockCnt(Users user) {
		SuspensionPeriodEnum period = SuspensionPeriodEnum.getByBlockCnt(user.getBlockCnt());
		if (period != null) {
			LocalDateTime newSuspensionEndTime = LocalDateTime.now().plusDays(period.getDays());
			user.setSuspensionEndTime(newSuspensionEndTime);
		}
	}

	//@Transactional
	public void checkBlockedCnt(Users user) {
		if (user.getBlockCnt().equals(UserConstant.BLOCKED_LIMIT)) {
			banUser(user);
		}
	}

	//@Transactional
	public void banUser(Users user) {
		user.setAccountStatusToBanned();
	}
}
