package swmaestro.spaceodyssey.weddingmate.domain.item.controller;

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
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.service.ItemsService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/portfolio/item")
public class ItemsController {

	private final ItemsService itemsService;
	@PostMapping("/save")
	@ResponseStatus(HttpStatus.CREATED)
	public ApiResponse<Object> createItem(@RequestBody ItemSaveReqDto itemSaveReqDto) {
		itemsService.createItem(itemSaveReqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(ITEM_CREATE_SUCCESS)
			.build();
	}

	@GetMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> getItemById(@AuthUsers Users users, @PathVariable("id") Long id) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(itemsService.findById(users, id))
			.build();
	}

	@PutMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> updateItem(@AuthUsers Users users, @PathVariable("id") Long id, @RequestBody ItemUpdateReqDto itemUpdateReqDto) {
		itemsService.updateItem(users, id, itemUpdateReqDto);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(ITEM_UPDATE_SUCCESS)
			.build();
	}

	@DeleteMapping("/{id}")
	@ResponseStatus(HttpStatus.OK)
	public ApiResponse<Object> deleteItem(@AuthUsers Users users, @PathVariable("id") Long id) {
		itemsService.deleteItem(users, id);
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(ITEM_DELETE_SUCCESS)
			.build();
	}

	@GetMapping("/search")
	public ApiResponse<Object> search(
		@PageableDefault(size = 18) Pageable pageable, @RequestParam(name = "keyword", required = false) String keyword) {

		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(itemsService.searchItems(pageable, keyword))
			.build();
	}


}