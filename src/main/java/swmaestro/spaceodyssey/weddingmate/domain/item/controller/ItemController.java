package swmaestro.spaceodyssey.weddingmate.domain.item.controller;

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
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemResDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.service.ItemService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/portfolio/item")
public class ItemController {

	private final ItemService itemService;
	@PostMapping("/save")
	public ResponseEntity<HttpStatus> createItem(@RequestBody ItemSaveReqDto itemSaveReqDto) {
		itemService.createItem(itemSaveReqDto);
		return ResponseEntity.ok().body(HttpStatus.CREATED);
	}

	@GetMapping("/{id}")
	public ResponseEntity<ItemResDto> getItemById(@PathVariable("id") Long id) {
		return ResponseEntity.ok().body(itemService.findById(id));
	}

	@PutMapping("/{id}")
	public ResponseEntity<HttpStatus> updateItem(@AuthUsers Users users, @PathVariable("id") Long id, @RequestBody ItemUpdateReqDto itemUpdateReqDto) {
		itemService.update(users, id, itemUpdateReqDto);
		return ResponseEntity.ok().body(HttpStatus.OK);
	}

	@DeleteMapping("/{id}")
	public ResponseEntity<HttpStatus> deleteItem(@AuthUsers Users users, @PathVariable("id") Long id) {
		itemService.delete(users, id);
		return ResponseEntity.ok().body(HttpStatus.OK);
	}
}
