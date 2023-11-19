package swmaestro.spaceodyssey.weddingmate.domain.community.post.controller;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.service.PostService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Tag(name = "Community Post API", description = "커뮤니티 게시글 관련 API")
@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/community/post")
public class PostsController {

	private final PostService postService;

	@Operation(summary = "유저가 업로드한 커뮤니티 게시물 저장")
	@PostMapping("/save")
	@ResponseStatus(HttpStatus.CREATED)
	public ApiResponse<Object> createPost(@AuthUsers Users users,
		@RequestBody PostSaveReqDto postSaveReqDto) {
		postService.createPost(users, postSaveReqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(POST_CREATE_SUCCESS)
			.build();
	}

	@Operation(summary = "커뮤니티 게시물 디테일")
	@GetMapping ("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPostDetail(@AuthUsers Users users, @PathVariable("id") Long id) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(postService.getPostDetail(users, id))
			.build();
	}

	@Operation(summary = "커뮤니티 게시물 리스트")
	@GetMapping ("/list")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPosts(@AuthUsers Users users, @PageableDefault(size = 10) Pageable pageable) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(postService.getPostList(pageable))
			.build();
	}

	@Operation(summary = "커뮤니티 게시물 인기순 리스트")
	@GetMapping ("/list/popular")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getPopularPosts(@AuthUsers Users users, @PageableDefault(size = 10) Pageable pageable) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(postService.getPostListSortedByViews(pageable))
			.build();
	}

	@Operation(summary = "id값으로 해당하는 Post 수정")
	@PutMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updatePost(@AuthUsers Users users, @PathVariable("id") Long id,
		@RequestBody PostUpdateReqDto postUpdateReqDto) {
		postService.updatePost(users, id, postUpdateReqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(POST_UPDATE_SUCCESS)
			.build();
	}

	@Operation(summary = "id값으로 해당하는 Post 삭제")
	@DeleteMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> deletePost(@AuthUsers Users users, @PathVariable("id") Long id) {
		postService.deletePost(users, id);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(POST_DELETE_SUCCESS)
			.build();
	}
}
