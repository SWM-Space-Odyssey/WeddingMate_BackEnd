package swmaestro.spaceodyssey.weddingmate.domain.file.service;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Objects;

import org.springframework.stereotype.Service;
import org.springframework.util.StreamUtils;
import org.springframework.web.multipart.MultipartFile;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.ItemFileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.enums.FilePathType;
import swmaestro.spaceodyssey.weddingmate.domain.file.enums.ImageExtensionType;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FileRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.config.S3.S3Uploader;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileKakaoProfileDownloadFailureException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileMalformedUrlException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileUnSupportedExtensionTypeException;

@Service
@RequiredArgsConstructor
@Transactional
public class FileUploadService {

	private final S3Uploader s3Uploader;
	private final FileService fileService;
	private final FileRepository fileRepository;

	private static final String FILE_EXTENSION_SEPARATOR = ".";

	/*================== User Profile ==================*/

	public File createUserProfile(String providerId, String imageUrl) {

		FileInfoDto fileInfoDto = extractFileNameAndExtension(imageUrl);
		String fileImagePath = buildProfileFilePath(fileInfoDto.getFileExtension(), providerId);

		byte[] imageData = dowmloadBasicProfileImage(imageUrl);
		return uploadImageDataToS3AndSaveInRepository(imageData, fileInfoDto, fileImagePath);
	}

	public String updateProfileFile(Users users, MultipartFile multipartFile) {
		FileInfoDto fileInfoDto = validateImageAndExtractFileInfo(multipartFile);
		String fileImagePath = buildProfileFilePath(fileInfoDto.getFileExtension(), users.getAuthProviderId());

		File file = uploadFileToS3AndSaveInRepository(multipartFile, fileInfoDto, fileImagePath);
		return file.getUrl();
	}

	public String deleteProfileFile(Users users) {
		File file = fileService.findById(users.getProfileImage().getFileId());

		// TODO : default 이미지를 기존 파일 대신 업로드

		return file.getUrl();
	}

	/*================== Item, Portfolio ==================*/

	public String uploadItemFile(ItemFileReqDto itemFileReqDto, MultipartFile multipartFile) {
		FileInfoDto fileInfoDto = validateImageAndExtractFileInfo(multipartFile);
		String fileImagePath = buildItemFilePath(fileInfoDto.getFileExtension(), itemFileReqDto);

		File file = uploadFileToS3AndSaveInRepository(multipartFile, fileInfoDto, fileImagePath);
		return file.getUrl();
	}

	public File uploadPortfolioFile(MultipartFile multipartFile, Portfolio portfolio) {
		FileInfoDto fileInfoDto = validateImageAndExtractFileInfo(multipartFile);
		String portfolioImagePath = buildPortfolioFilePath(fileInfoDto.getFileExtension(), portfolio.getPortfolioId());

		return uploadFileToS3AndSaveInRepository(multipartFile, fileInfoDto, portfolioImagePath);
	}

	/*================== 중간 함수 ==================*/

	private FileInfoDto validateImageAndExtractFileInfo(final MultipartFile multipartFile) {
		validateFileIsImage(multipartFile);
		return extractFileNameAndExtension(Objects.requireNonNull(multipartFile.getOriginalFilename()));
	}

	private File uploadFileToS3AndSaveInRepository(final MultipartFile multipartFile, FileInfoDto fileInfoDto,
		String fileImagePath) {
		s3Uploader.uploadS3(multipartFile, fileImagePath);
		return createAndSaveFileInRepository(fileInfoDto, fileImagePath);
	}

	private File uploadImageDataToS3AndSaveInRepository(byte[] imageData, FileInfoDto fileInfoDto,
		String fileImagePath) {
		s3Uploader.uploadByteImageToS3(imageData, fileImagePath);
		return createAndSaveFileInRepository(fileInfoDto, fileImagePath);
	}

	/*================== File Path ==================*/

	private String buildProfileFilePath(String ext, String providerId) {
		return FilePathType.PROFILE.getPath()
			+ providerId
			+ ext;
	}

	private String buildPortfolioFilePath(String ext, Long portfolioId) {
		return FilePathType.PORTFOLIO.getPath()
			+ portfolioId
			+ "/"
			+ portfolioId
			+ "_rep"
			+ ext;
	}

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

	/*================== Basic Upload Service ==================*/

	private File createAndSaveFileInRepository(FileInfoDto fileInfoDto, String s3FileName) {
		File file = File.builder()
			.filename(fileInfoDto.getFileName())
			.filetype(fileInfoDto.getFileExtension())
			.url(s3FileName)
			.build();

		fileRepository.save(file);

		return file;
	}

	private FileInfoDto extractFileNameAndExtension(String originalFileName) {
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
