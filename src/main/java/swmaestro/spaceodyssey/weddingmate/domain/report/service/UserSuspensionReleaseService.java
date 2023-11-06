package swmaestro.spaceodyssey.weddingmate.domain.report.service;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.service.repositoryservice.UsersRepositoryService;

import java.time.LocalDateTime;
import java.util.List;

@Service
public class UserSuspensionReleaseService {

	private UsersRepositoryService usersRepositoryService;
	private UsersRepository usersRepository;

	@Scheduled(cron = "0 0 0 * * ?") // 매일 자정에 실행
	public void checkAndReleaseSuspendedUsers() {
		List<Users> suspendedUserList = usersRepositoryService.findAllSuspendedUsers();

		LocalDateTime now = LocalDateTime.now();

		for (Users user : suspendedUserList) {
			if (user.getSuspensionEndTime() != null
					&& user.getSuspensionEndTime().isBefore(now)) {
				user.setAccountStatusToNormal();
				user.setSuspensionEndTime(null);

				usersRepository.save(user);
			}
		}
	}
}
