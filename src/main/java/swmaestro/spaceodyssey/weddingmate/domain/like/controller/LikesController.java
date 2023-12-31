package swmaestro.spaceodyssey.weddingmate.domain.like.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import java.util.Map;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;

import swmaestro.spaceodyssey.weddingmate.domain.like.dto.LikeReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.service.LikesService;
import swmaestro.spaceodyssey.weddingmate.domain.like.service.LikeActionService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Like API", description = "좋아요 관련 API")
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/likes")
public class LikesController {


	private final LikeActionService likesService;
	private final Map<String, LikesService> likeServiceMap;

	@Operation(summary = "유저가 좋아요한 포트폴리오/아이템/플래너/회사 목록 제공")
	@GetMapping("/by-user-id/{service}")
	public ApiResponse<Object> getUserLiked(@AuthUsers Users users, @NotNull @PathVariable("service") LikeEnum service) {

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data((likeServiceMap.get(service.getServiceName()).getUserLiked(users)))
			.build();
	}

	@Operation(summary = "유저의 좋아요 반영(포트폴리오/아이템/플래너/회사)")
	@PostMapping()
	public ApiResponse<Object> like(@AuthUsers Users users, @RequestBody LikeReqDto likeReqDto) {
		Boolean checkIsLiked = likesService.checkIsLiked(likeReqDto.getId(), users, likeReqDto.getLikeType());

		if (Boolean.TRUE.equals(checkIsLiked)) {
			likesService.unlike(likeReqDto.getId(), users, likeReqDto.getLikeType());
		} else {
			likesService.like(likeReqDto.getId(), users, likeReqDto.getLikeType());
		}

		String resultMessage = Boolean.TRUE.equals(checkIsLiked) ? UNLIKE_SUCCESS : LIKE_SUCCESS;

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(resultMessage)
			.build();
	}

}
