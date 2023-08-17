package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.enums.FilePathType;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileService;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;

@Service
@RequiredArgsConstructor
@Transactional
public class ProfileFileUploadService {

	private final FileUploadService fileUploadService;
	private final FileService fileService;
	private final UsersRepository usersRepository;

	public File createUserProfile(String providerId, String imageUrl) {

		FileInfoDto fileInfoDto = fileUploadService.extractFileNameAndExtension(imageUrl);
		String fileImagePath = buildProfileFilePath(fileInfoDto.getFileExtension(), providerId);

		byte[] imageData = fileUploadService.dowmloadBasicProfileImage(imageUrl);
		return fileUploadService.uploadImageDataToS3AndSaveInRepository(imageData, fileInfoDto, fileImagePath);
	}

	@Transactional
	public String updateProfileFile(Users users, MultipartFile multipartFile) {
		FileInfoDto fileInfoDto = fileUploadService.validateImageAndExtractFileInfo(multipartFile);
		String fileImagePath = buildProfileFilePath(fileInfoDto.getFileExtension(), users.getAuthProviderId());

		File file = fileUploadService.uploadFileToS3AndSaveInRepository(multipartFile, fileInfoDto, fileImagePath);
		users.updateProfileImage(file);
		usersRepository.save(users);

		return file.getUrl();
	}

	public String deleteProfileFile(Users users) {
		File file = fileService.findById(users.getProfileImage().getFileId());

		// TODO : default 이미지를 기존 파일 대신 업로드

		return file.getUrl();
	}

	/*================== File Path ==================*/
	private String buildProfileFilePath(String ext, String providerId) {
		return FilePathType.PROFILE.getPath()
			+ providerId
			+ ext;
	}
}
