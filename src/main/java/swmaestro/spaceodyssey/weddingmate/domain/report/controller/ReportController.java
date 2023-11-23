package swmaestro.spaceodyssey.weddingmate.domain.report.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import swmaestro.spaceodyssey.weddingmate.domain.report.dto.ReportReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.report.service.ReportService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.REPORT_CREATE_SUCCESS;

@Tag(name = "Report API", description = "신고 관련 API")
@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/report")
public class ReportController {

	private final ReportService reportService;

	@Operation(summary = "유저의 신고 반영(포트폴리오/아이템/타 유저)")
	@PostMapping()
	public ApiResponse<Object> makeReport(@AuthUsers Users user,
										  @RequestBody ReportReqDto reqDto) {
		reportService.makeReport(user, reqDto);
		return ApiResponse.builder()
				.status(ApiResponseStatus.SUCCESS)
				.data(REPORT_CREATE_SUCCESS)
				.build();
	}

	@Operation(summary = "해당 유저가 신고한 내역 조회")
	@GetMapping("/by-reporter-user-id/{userId}")
	public ApiResponse<Object> getReportsByReporterUserId(@AuthUsers Users user,
														  @NotNull @PathVariable("userId") Long userId) {
		return ApiResponse.builder()
				.status(ApiResponseStatus.SUCCESS)
				.data(reportService.getReportsByReporterUserId(user, userId))
				.build();
	}

	@Operation(summary = "해당 유저가 신고당한 내역 조회")
	@GetMapping("/by-reported-user-id/{userId}")
	public ApiResponse<Object> getReportsByReportedUserId(@AuthUsers Users user,
														  @NotNull @PathVariable("userId") Long userId) {
		return ApiResponse.builder()
				.status(ApiResponseStatus.SUCCESS)
				.data(reportService.getReportsByReportedUserId(user, userId))
				.build();
	}

}
