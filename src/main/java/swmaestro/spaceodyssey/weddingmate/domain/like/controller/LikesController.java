package swmaestro.spaceodyssey.weddingmate.domain.like.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.LikeReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.service.LikesService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;
import swmaestro.spaceodyssey.weddingmate.global.exception.like.LikeTypeNotSupportedException;

@Tag(name = "Like API", description = "좋아요 관련 API")
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/like")
public class LikesController {

	@Autowired
	private LikesService likesService;

	@Operation(summary = "유저가 좋아요한 포트폴리오 목록 제공")
	@GetMapping("/portfolio")
	public ApiResponse<Object> getUserLikedPortfolio(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data((likesService.getUserLikedPortfolio(users)))
			.build();
	}

	@Operation(summary = "유저가 좋아요한 아이템 목록 제공")
	@GetMapping("/item")
	public ApiResponse<Object> getUserLikedItem(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data((likesService.getUserLikedItem(users)))
			.build();
	}

	@Hidden
	@Operation(summary = "유저가 좋아요한 플래너 목록 제공")
	@GetMapping("/planner")
	public ApiResponse<Object> getUserLikedPlanner(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data((likesService.getUserLikedPlanner(users)))
			.build();
	}

	@Operation(summary = "유저가 좋아요한 업체 목록 제공")
	@GetMapping("/company")
	public ApiResponse<Object> getUserLikedCompany(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data((likesService.getUserLikedCompany(users)))
			.build();
	}

	@Operation(summary = "유저의 좋아요 반영(포트폴리오, 아이템, 업체)")
	@PostMapping("")
	public ApiResponse<Object> like(@AuthUsers Users users, @RequestBody LikeReqDto likeReqDto) {
		LikeEnum likeType = getLikeTypeFromDto(likeReqDto);
		Boolean isLiked = likesService.like(likeReqDto.getId(), users, likeType);

		String resultMessage = Boolean.TRUE.equals(isLiked) ? LIKE_SUCCESS : UNLIKE_SUCCESS;

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resultMessage)
			.build();
	}

	private LikeEnum getLikeTypeFromDto(LikeReqDto likeReqDto) {
		String likeType = likeReqDto.getLikeType();

		return switch (likeType) {
			case "PORTFOLIO" -> LikeEnum.PORTFOLIO;
			case "ITEM" -> LikeEnum.ITEM;
			case "PLANNER" -> LikeEnum.PLANNER;
			case "COMPANY" -> LikeEnum.COMPANY;
			default -> throw new LikeTypeNotSupportedException();
		};
	}

}

