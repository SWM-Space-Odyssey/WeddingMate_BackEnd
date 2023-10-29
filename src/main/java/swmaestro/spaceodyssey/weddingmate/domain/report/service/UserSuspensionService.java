package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.SuspensionPeriodEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserAccountStatusEnum;
import swmaestro.spaceodyssey.weddingmate.global.constant.UserConstant;

import java.time.LocalDateTime;

@Service
@Transactional
public class UserSuspensionService {

	public Users addReportCnt(Users user) {
		user.incrementReportCnt();
		checkReportCnt(user);
		return user;
	}

	private void checkReportCnt(Users user) {
		if (user.getReportCnt() == UserConstant.REPORT_LIMIT) {
			user.resetReportCnt();
			blockUser(user);
		}
	}

	private void blockUser(Users user) {
		user.setAccountStatusToSuspended();
		user.incrementBlockCnt();
		checkBlockedCnt(user);

		if (!user.getAccountStatus().equals(UserAccountStatusEnum.BANNED)) {
			setSuspensionPeriodByBlockCnt(user);
		}
	}

	private void setSuspensionPeriodByBlockCnt(Users user) {
		SuspensionPeriodEnum period = SuspensionPeriodEnum.getByBlockCnt(user.getBlockCnt());
		if (period != null) {
			LocalDateTime newSuspensionEndTime = LocalDateTime.now().plusDays(period.getDays());
			user.setSuspensionEndTime(newSuspensionEndTime);
		}
	}

	private void checkBlockedCnt(Users user) {
		if (user.getBlockCnt().equals(UserConstant.BLOCKED_LIMIT)) {
			banUser(user);
		}
	}

	private void banUser(Users user) {
		user.setAccountStatusToBanned();
	}
}
