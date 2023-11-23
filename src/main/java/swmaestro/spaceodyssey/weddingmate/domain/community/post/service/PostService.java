package swmaestro.spaceodyssey.weddingmate.domain.community.post.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostDetailResDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.dto.PostUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.entity.Posts;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.mapper.PostMapper;
import swmaestro.spaceodyssey.weddingmate.domain.community.post.repository.PostsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

@Service
@RequiredArgsConstructor
@Transactional
public class PostService {

	private final PostMapper postMapper;
	private final PostsRepository postsRepository;
	private final PostRepositoryService postRepositoryService;

	public Long createPost(Users user, PostSaveReqDto postSaveReqDto) {
		Posts post = postMapper.dtoToEntity(user, postSaveReqDto);

		postsRepository.save(post);

		return post.getPostId();
	}

	public Long updatePost(Users user, Long postId, PostUpdateReqDto updateReqDto) {
		Posts post = postRepositoryService.findPostById(postId);

		verifyUserIsWriter(post, user);

		post.updatePost(updateReqDto.getTitle(), updateReqDto.getCategory(), updateReqDto.getContent());

		return post.getPostId();
	}

	public void deletePost(Users user, Long postId) {
		Posts post = postRepositoryService.findPostById(postId);

		verifyUserIsWriter(post, user);

		post.deletePost();
	}

	public PostDetailResDto getPostDetail(Users user, Long id) {
		Posts post = postRepositoryService.findPostById(id);

		increaseViewCount(post);

		Boolean isWriter = checkUserIsWriter(post, user);

		return postMapper.entityToDto(post, isWriter);
	}

	public Page<PostListResDto> getPostList(Pageable pageable) {
		Page<Posts> postPage = postsRepository.findAll(pageable);

		return postPage.map(postMapper::entityToListDto);
	}

	public Page<PostListResDto> getPostListSortedByViews(Pageable pageable) {
		pageable = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize(), Sort.by("viewCount").descending());

		Page<Posts> postPage = postsRepository.findAll(pageable);

		return postPage.map(postMapper::entityToListDto);
	}

	private void increaseViewCount(Posts post) {
		post.addViewCount();
	}
	/*================== 예외 처리 ==================*/
	public void verifyUserIsWriter(Posts post, Users users) {
		if (!post.getUsers().getUserId().equals(users.getUserId())) {
			throw new UserUnAuthorizedException();
		}
	}

	public Boolean checkUserIsWriter(Posts post, Users users) {
		return post.getUsers().getUserId().equals(users.getUserId());
	}
}
