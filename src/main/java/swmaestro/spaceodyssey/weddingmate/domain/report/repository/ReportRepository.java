package swmaestro.spaceodyssey.weddingmate.domain.report.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import swmaestro.spaceodyssey.weddingmate.domain.report.entity.Report;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

import java.util.List;

public interface ReportRepository extends JpaRepository<Report, Long> {
	List<Report> findByReporterUser(Users user);

	List<Report> findByReportedUser(Users user);
}
