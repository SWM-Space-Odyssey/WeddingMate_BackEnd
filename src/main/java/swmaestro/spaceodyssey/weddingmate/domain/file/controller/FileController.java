package swmaestro.spaceodyssey.weddingmate.domain.file.controller;

import java.io.IOException;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileService;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/file")
public class FileController {

	private final FileService fileService;

	@PostMapping
	public ResponseEntity<String> post(@RequestPart FileReqDto fileReqDto, @NotNull @RequestPart("file") MultipartFile multipartFile) throws
		IOException {
		return ResponseEntity.ok().body(fileService.uploadItemImage(fileReqDto, multipartFile));
	}
}
