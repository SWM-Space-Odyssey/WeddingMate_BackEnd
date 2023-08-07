package swmaestro.spaceodyssey.weddingmate.domain.file.controller;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.ImageListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileService;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileUploadService;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/file")
public class FileController {

	private final FileUploadService fileUploadService;
	private final FileService fileService;

	@PostMapping("/items")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<String> postItem(@RequestPart FileReqDto fileReqDto, @NotNull @RequestPart("file") MultipartFile multipartFile) {
		return ResponseEntity.ok().body(fileUploadService.uploadItemFile(fileReqDto, multipartFile));
	}

	@GetMapping
	public ResponseEntity<Page<ImageListResDto>> getImagePage(@PageableDefault(size = 18) Pageable pageable) {
		return ResponseEntity.ok().body(fileService.getImage(pageable));
	}
}
