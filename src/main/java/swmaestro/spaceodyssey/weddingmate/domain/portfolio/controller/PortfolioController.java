package swmaestro.spaceodyssey.weddingmate.domain.portfolio.controller;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.service.PortfolioService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/portfolio")
public class PortfolioController {

	private final PortfolioService portfolioService;

	@PostMapping("/save")
	public ResponseEntity<HttpStatus> createPortfolio(@AuthUsers Users users,
															@RequestBody PortfolioSaveReqDto portfolioSaveReqDto) {
		portfolioService.createPortfolio(users, portfolioSaveReqDto);
		return ResponseEntity.ok().body(HttpStatus.CREATED);
	}

	@GetMapping("/")
	public ResponseEntity<List<PortfolioListResDto>> getPortfolioByUser(@AuthUsers Users users) {
		return  ResponseEntity.ok().body(portfolioService.findByUser(users.getUserId()));
	}

	@GetMapping("/{id}")
	public ResponseEntity<PortfolioDetailResDto> getPortfolioById(@PathVariable("id") Long id) {
		return ResponseEntity.ok().body(portfolioService.findById(id));
	}

	@PutMapping("/{id}")
	public ResponseEntity<HttpStatus> updatePortfolio(@AuthUsers Users users, @PathVariable("id") Long id,
																	@RequestBody PortfolioUpdateReqDto portfolioUpdateReqDto) {
		portfolioService.updatePortfolio(users, id, portfolioUpdateReqDto);
		return ResponseEntity.ok().body(HttpStatus.OK);
	}

	@DeleteMapping("/{id}")
	public ResponseEntity<HttpStatus> deletePortfolio(@AuthUsers Users users, @PathVariable("id") Long id) {
		portfolioService.deletePortfolio(users, id);
		return ResponseEntity.ok().body(HttpStatus.OK);
	}
}
