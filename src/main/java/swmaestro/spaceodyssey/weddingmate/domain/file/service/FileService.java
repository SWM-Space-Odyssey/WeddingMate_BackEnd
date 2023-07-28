package swmaestro.spaceodyssey.weddingmate.domain.file.service;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.ObjectMetadata;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.dto.FileReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FileRepository;

@Service
@RequiredArgsConstructor
@Transactional
public class FileService {

	private final AmazonS3Client amazonS3Client;
	private final FileRepository fileRepository;

	@Value("${cloud.aws.s3.bucket}")
	private String bucket;

	public ArrayList<String> splitFileName(String originalFileName) {
		int fileExtensionIndex = originalFileName.lastIndexOf(".");
		String ext = originalFileName.substring(fileExtensionIndex);
		String fileName = originalFileName.substring(0, fileExtensionIndex - 1);

		return new ArrayList<>(Arrays.asList(fileName, ext));
	}

	public String buildItemFileName(String ext, FileReqDto fileReqDto) {
		Calendar cal = Calendar.getInstance();
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmSS");
		String time = dateFormat.format(cal.getTime());
		return new StringBuilder().append("portfolio/")
			.append(fileReqDto.getPortfolioId())
			.append("/")
			.append(fileReqDto.getItemCategory())
			.append("/")
			.append(time)
			.append(ext)
			.toString();
	}

	public String buildPortfolioFileName(String ext, Long portfolioId) {
		return new StringBuilder().append("portfolio/")
			.append(portfolioId)
			.append("/")
			.append(portfolioId)
			.append("_rep")
			.append(ext)
			.toString();
	}

	public String uploadItemImage(FileReqDto fileReqDto, MultipartFile multipartFile) throws IOException {
		ArrayList<String> fileInfo = splitFileName(multipartFile.getOriginalFilename());
		String fileName = fileInfo.get(0);
		String fileExtension = fileInfo.get(1);
		String s3FileName = buildItemFileName(fileExtension, fileReqDto);

		uploadS3(multipartFile, s3FileName);

		File file = File.builder().filename(fileName).filetype(fileExtension).url(s3FileName).build();

		fileRepository.save(file);

		return s3FileName;
	}

	public File uploadPortfolioImage(MultipartFile multipartFile, Long portfolioId) throws IOException {
		ArrayList<String> fileInfo = splitFileName(multipartFile.getOriginalFilename());
		String fileName = fileInfo.get(0);
		String fileExtension = fileInfo.get(1);
		String s3FileName = buildPortfolioFileName(fileExtension, portfolioId);

		uploadS3(multipartFile, s3FileName);

		File file = File.builder().filename(fileName).filetype(fileExtension).url(s3FileName).build();

		fileRepository.save(file);

		return  file;
	}

	public void uploadS3(MultipartFile multipartFile, String s3FileName) throws IOException {
		ObjectMetadata metadata = new ObjectMetadata();
		metadata.setContentType(multipartFile.getContentType());
		metadata.setContentLength(multipartFile.getSize());

		amazonS3Client.putObject(bucket, s3FileName, multipartFile.getInputStream(), metadata);
	}
}
