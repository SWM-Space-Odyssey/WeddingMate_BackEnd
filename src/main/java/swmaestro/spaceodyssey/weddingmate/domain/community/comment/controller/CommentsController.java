package swmaestro.spaceodyssey.weddingmate.domain.community.comment.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.dto.CommentSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.comment.service.CommentService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Community API", description = "커뮤니티 관련 API")
@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/community/post")
public class CommentsController {

	private final CommentService commentService;

	@Operation(summary = "커뮤니티 게시물 댓글 저장")
	@PostMapping("/{id}/comment")
	@ResponseStatus(HttpStatus.CREATED)
	public ApiResponse<Object> createComment(@AuthUsers Users users,
		@PathVariable("id") Long id,
		@RequestBody CommentSaveReqDto commentSaveReqDto) {
		commentService.createComment(users, id, commentSaveReqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(COMMENT_CREATE_SUCCESS)
			.build();
	}

	@Operation(summary = "id값으로 해당하는 댓글 삭제")
	@DeleteMapping("/comment/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> deletePost(@AuthUsers Users users, @PathVariable("id") Long id) {
		commentService.deleteComment(users, id);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(COMMENT_DELETE_SUCCESS)
			.build();
	}
}
