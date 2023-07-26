package swmaestro.spaceodyssey.weddingmate.domain.category.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.category.dto.CategoryResDto;
import swmaestro.spaceodyssey.weddingmate.domain.category.service.CategoryService;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/category")
public class CategoryController {

	private final CategoryService categoryService;

	@GetMapping("/all")
	public ResponseEntity<List<CategoryResDto>> getAllCategory() {
		return  ResponseEntity.ok().body(categoryService.findAllCategory());
	}
}
