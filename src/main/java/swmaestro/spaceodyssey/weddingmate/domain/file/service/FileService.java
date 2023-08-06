package swmaestro.spaceodyssey.weddingmate.domain.file.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.ImageListResDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FileRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotFoundException;

@Service
@RequiredArgsConstructor
@Transactional
public class FileService {

	private final FileRepository fileRepository;
	private final PortfolioRepository portfolioRepository;

	public Page<ImageListResDto> getImage(Pageable pageable) {

		List<ImageListResDto> fileList = new ArrayList<>();

		fileRepository.findAll().forEach(file -> {
			if (file.getItem() != null) {
				if (Boolean.FALSE.equals(file.getItem().getIsDeleted())) {
					fileList.add(new ImageListResDto(file.getUrl(), file.getItem().getItemId(), null));
				}
			} else {
				Optional<Portfolio> portfolio = portfolioRepository.findByFile(file);
				if (portfolio.isPresent() && Boolean.FALSE.equals(portfolio.get().getIsDeleted())) {
					fileList.add(new ImageListResDto(file.getUrl(), null, portfolio.get().getPortfolioId()));
				}
			}
		});

		//이미지 랜덤하게 섞기
		Collections.shuffle(fileList);

		//list를 page로 변환
		int start = (int)pageable.getOffset();
		int end = Math.min((start + pageable.getPageSize()), fileList.size());

		return new PageImpl<>(fileList.subList(start, end), pageable, fileList.size());
	}

	public File findById(Long fileId) {
		return fileRepository.findById(fileId)
			.orElseThrow(FileNotFoundException::new);
	}
}
