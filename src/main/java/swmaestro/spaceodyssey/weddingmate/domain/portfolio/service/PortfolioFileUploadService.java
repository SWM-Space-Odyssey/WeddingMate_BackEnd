package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.enums.FilePathType;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FilesUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional
public class PortfolioFileUploadService {

	private final FilesUploadService fileUploadService;

	public Files uploadPortfolioFile(MultipartFile multipartFile, Portfolios portfolios) {
		FileInfoDto fileInfoDto = fileUploadService.validateImageAndExtractFileInfo(multipartFile);
		String portfolioImagePath = buildPortfolioFilePath(fileInfoDto.getFileExtension(), portfolios.getPortfolioId());
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
