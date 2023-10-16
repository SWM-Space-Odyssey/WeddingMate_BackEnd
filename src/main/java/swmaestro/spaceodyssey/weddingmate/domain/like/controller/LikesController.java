package swmaestro.spaceodyssey.weddingmate.domain.like.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import java.util.Map;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.LikeReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.service.LikeService;
import swmaestro.spaceodyssey.weddingmate.domain.like.service.LikesService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/like")
public class LikesController {


	private final LikesService likesService;
	private final Map<String, LikeService> likeServiceMap;


	@GetMapping("/{service}")
	public ApiResponse<Object> getUserLiked(@AuthUsers Users users, @NotNull @PathVariable("service") LikeEnum service) {

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data((likeServiceMap.get(service.getServiceName()).getUserLiked(users)))
			.build();
	}

	@PostMapping("")
	public ApiResponse<Object> like(@AuthUsers Users users, @RequestBody LikeReqDto likeReqDto) {
		Boolean isLiked = likesService.like(likeReqDto.getId(), users, likeReqDto.getLikeType());

		String resultMessage = Boolean.TRUE.equals(isLiked) ? LIKE_SUCCESS : UNLIKE_SUCCESS;

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resultMessage)
			.build();
	}

}

