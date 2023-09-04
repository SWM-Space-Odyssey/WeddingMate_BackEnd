package swmaestro.spaceodyssey.weddingmate.domain.item.service;

import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemFileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.enums.FilePathType;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FilesUploadService;

@Service
@RequiredArgsConstructor
@Transactional
public class ItemsFilesUploadService {

	private final FilesUploadService fileUploadService;

	public String uploadItemFile(ItemFileReqDto itemFileReqDto, MultipartFile multipartFile) {
		FileInfoDto fileInfoDto = fileUploadService.validateImageAndExtractFileInfo(multipartFile);
		String fileImagePath = buildItemFilePath(fileInfoDto.getFileExtension(), itemFileReqDto);

		Files files = fileUploadService.uploadFileToS3AndSaveInRepository(multipartFile, fileInfoDto, fileImagePath);
		return files.getUrl();
	}

	/*================== File Path ==================*/
	private String buildItemFilePath(String ext, ItemFileReqDto itemFileReqDto) {
		Calendar cal = Calendar.getInstance();
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmSS");
		String time = dateFormat.format(cal.getTime());
		return FilePathType.PORTFOLIO.getPath()
			+ itemFileReqDto.getPortfolioId()
			+ "/"
			+ itemFileReqDto.getItemCategory()
			+ "/"
			+ time
			+ ext;
	}
}
