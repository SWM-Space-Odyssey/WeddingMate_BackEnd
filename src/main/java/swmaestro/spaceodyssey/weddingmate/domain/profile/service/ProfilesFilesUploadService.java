package swmaestro.spaceodyssey.weddingmate.domain.profile.service;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileInfoDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.enums.FilePathType;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FilesService;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FilesUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;

@Service
@RequiredArgsConstructor
@Transactional
public class ProfilesFilesUploadService {

	private final FilesUploadService fileUploadService;
	private final FilesService fileService;
	private final UsersRepository usersRepository;

	public Files createUserProfile(String providerId, String imageUrl) {

		FileInfoDto fileInfoDto = fileUploadService.extractFileNameAndExtension(imageUrl);
		String fileImagePath = buildProfileFilePath(fileInfoDto.getFileExtension(), providerId);

		byte[] imageData = fileUploadService.dowmloadBasicProfileImage(imageUrl);
		return fileUploadService.uploadImageDataToS3AndSaveInRepository(imageData, fileInfoDto, fileImagePath);
	}

	@Transactional
	public String updateProfileFile(Users users, MultipartFile multipartFile) {
		FileInfoDto fileInfoDto = fileUploadService.validateImageAndExtractFileInfo(multipartFile);
		String fileImagePath = buildProfileFilePath(fileInfoDto.getFileExtension(), users.getAuthProviderId());

		Files files = fileUploadService.uploadFileToS3AndSaveInRepository(multipartFile, fileInfoDto, fileImagePath);
		users.updateProfileImage(files);
		usersRepository.save(users);

		return files.getUrl();
	}

	public String deleteProfileFile(Users users) {
		Files files = fileService.findById(users.getProfileImage().getFileId());

		// TODO : default 이미지를 기존 파일 대신 업로드

		return files.getUrl();
	}

	/*================== File Path ==================*/
	private String buildProfileFilePath(String ext, String providerId) {
		return FilePathType.PROFILE.getPath()
			+ providerId
			+ ext;
	}
}
