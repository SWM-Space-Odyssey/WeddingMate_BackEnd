package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.enums.FilePathType;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional
public class PortfolioFileUploadService {

	private final FileUploadService fileUploadService;

	public File uploadPortfolioFile(MultipartFile multipartFile, Portfolio portfolio) {
		FileInfoDto fileInfoDto = fileUploadService.validateImageAndExtractFileInfo(multipartFile);
		String portfolioImagePath = buildPortfolioFilePath(fileInfoDto.getFileExtension(), portfolio.getPortfolioId());
		return fileUploadService.uploadFileToS3AndSaveInRepository(multipartFile, fileInfoDto, portfolioImagePath);
	}

	/*================== File Path ==================*/
	private String buildPortfolioFilePath(String ext, Long portfolioId) {
		return FilePathType.PORTFOLIO.getPath()
			+ portfolioId
			+ "/"
			+ portfolioId
			+ "_rep"
			+ ext;
	}
}
