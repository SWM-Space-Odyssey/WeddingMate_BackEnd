package swmaestro.spaceodyssey.weddingmate.domain.file.service;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Objects;

import org.springframework.stereotype.Service;
import org.springframework.util.StreamUtils;
import org.springframework.web.multipart.MultipartFile;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.enums.ImageExtensionType;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FileRepository;
import swmaestro.spaceodyssey.weddingmate.global.config.S3.S3Uploader;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileKakaoProfileDownloadFailureException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileMalformedUrlException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileUnSupportedExtensionTypeException;

@Service
@RequiredArgsConstructor
@Transactional
public class FileUploadService {

	private final S3Uploader s3Uploader;
	private final FileRepository fileRepository;

	private static final String FILE_EXTENSION_SEPARATOR = ".";

	/*================== 중간 함수 ==================*/

	public FileInfoDto validateImageAndExtractFileInfo(final MultipartFile multipartFile) {
		validateFileIsImage(multipartFile);
		return extractFileNameAndExtension(Objects.requireNonNull(multipartFile.getOriginalFilename()));
	}

	public File uploadFileToS3AndSaveInRepository(final MultipartFile multipartFile, FileInfoDto fileInfoDto,
		String fileImagePath) {
		s3Uploader.uploadS3(multipartFile, fileImagePath);
		return createAndSaveFileInRepository(fileInfoDto, fileImagePath);
	}

	public File uploadImageDataToS3AndSaveInRepository(byte[] imageData, FileInfoDto fileInfoDto,
		String fileImagePath) {
		s3Uploader.uploadByteImageToS3(imageData, fileImagePath);
		return createAndSaveFileInRepository(fileInfoDto, fileImagePath);
	}

	/*================== Basic Upload Service ==================*/

	public File createAndSaveFileInRepository(FileInfoDto fileInfoDto, String s3FileName) {
		File file = File.builder()
			.filename(fileInfoDto.getFileName())
			.filetype(fileInfoDto.getFileExtension())
			.url(s3FileName)
			.build();

		fileRepository.save(file);

		return file;
	}

	public FileInfoDto extractFileNameAndExtension(String originalFileName) {
		int fileExtensionIndex = originalFileName.lastIndexOf(FILE_EXTENSION_SEPARATOR);
		String fileName = originalFileName.substring(0, fileExtensionIndex - 1);
		String fileExtension = originalFileName.substring(fileExtensionIndex);

		validateFileExtensionType(fileExtension);

		return FileInfoDto.builder()
			.fileName(fileName)
			.fileExtension(fileExtension)
			.build();
	}

	public byte[] dowmloadBasicProfileImage(String imageUrl) {
		try {
			URL url = new URL(imageUrl);
			return StreamUtils.copyToByteArray(url.openStream());
		} catch (MalformedURLException e) {
			throw new FileMalformedUrlException(e.getMessage());
		} catch (IOException e) {
			throw new FileKakaoProfileDownloadFailureException(e.getMessage());
		}
	}

	/*================== Validation ==================*/

	private void validateFileExtensionType(String fileExtension) {
		if (!ImageExtensionType.isSupport(fileExtension)) {
			throw new FileUnSupportedExtensionTypeException();
		}
	}

	private void validateFileIsImage(final MultipartFile multipartFile) {
		ImageFileValidator.checkEmtpyFileName(multipartFile.getOriginalFilename());
		ImageFileValidator.checkImageType(multipartFile);
	}
}
