package swmaestro.spaceodyssey.weddingmate.domain.portfolio.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.service.PortfolioService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Portfolio API", description = "포트폴리오 관련 API")
@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/portfolio")
public class PortfoliosController {

	private final PortfolioService portfolioService;

	@Operation(summary = "유저가 업로드한 포트폴리오 저장")
	@PostMapping("/save")
	@ResponseStatus(HttpStatus.CREATED)
	public ApiResponse<Object> createPortfolio(@AuthUsers Users users,
		@NotNull @RequestPart("file") MultipartFile multipartFile,
		@RequestPart PortfolioSaveReqDto portfolioSaveReqDto) {
		portfolioService.createPortfolio(users, multipartFile, portfolioSaveReqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(PORTFOLIO_CREATE_SUCCESS)
			.build();
	}

	@Operation(summary = "유저가 올린 포트폴리오 목록 제공")
	@GetMapping("/")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPortfolioByUser(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(portfolioService.getPortfolioByUser(users.getUserId()))
			.build();
	}

	@Operation(summary = "id값으로 해당하는 포트폴리오 조회")
	@GetMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPortfolioById(@AuthUsers Users users, @PathVariable("id") Long id) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(portfolioService.getPortfolioDetail(users, id))
			.build();
	}

	@Operation(summary = "id값으로 해당하는 포트폴리오 수정(작성자 조회)")
	@PostMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updatePortfolio(@AuthUsers Users users, @PathVariable("id") Long id,
		@RequestPart(required = false, value = "file") MultipartFile multipartFile,
		@RequestPart PortfolioUpdateReqDto portfolioUpdateReqDto) {
		portfolioService.updatePortfolio(users, id, portfolioUpdateReqDto, multipartFile);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(PORTFOLIO_UPDATE_SUCCESS)
			.build();
	}

	@Operation(summary = "id값으로 해당하는 포트폴리오 삭제(작성자 조회)")
	@DeleteMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> deletePortfolio(@AuthUsers Users users, @PathVariable("id") Long id) {
		portfolioService.deletePortfolio(users, id);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(PORTFOLIO_DELETE_SUCCESS)
			.build();
	}
}