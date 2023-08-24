package swmaestro.spaceodyssey.weddingmate.domain.like.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.LikeReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.service.LikeService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;
import swmaestro.spaceodyssey.weddingmate.global.exception.like.LikeTypeNotSupportedException;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/like")
public class LikeController {


	@Autowired
	private LikeService likeService;

	@GetMapping("/portfolio")
	public ApiResponse<Object> getUserLikedPortfolio(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data((likeService.getUserLikedPortfolio(users)))
			.build();
	}

	@PostMapping("/")
	public ApiResponse<Object> like(@AuthUsers Users users, @RequestBody LikeReqDto likeReqDto) {
		LikeEnum likeType = getLikeTypeFromDto(likeReqDto);
		Boolean isLiked = likeService.like(likeReqDto.getId(), users, likeType);

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
			default -> throw new LikeTypeNotSupportedException();
		};
	}

}

