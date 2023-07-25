package swmaestro.spaceodyssey.weddingmate.domain.tag.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.tag.dto.TagResDto;
import swmaestro.spaceodyssey.weddingmate.domain.tag.service.TagService;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/tag")
public class TagController {
	private final TagService tagService;

	@GetMapping("/all")
	public ResponseEntity<List<TagResDto>> getAllTag(@RequestParam String category) {
		return  ResponseEntity.ok().body(tagService.findAllTag(category));
	}
}
