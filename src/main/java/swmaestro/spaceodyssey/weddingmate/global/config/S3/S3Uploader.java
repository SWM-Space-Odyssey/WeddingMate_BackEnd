package swmaestro.spaceodyssey.weddingmate.global.config.S3;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.ObjectMetadata;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileUploadFailureException;

@Component
@RequiredArgsConstructor
public class S3Uploader {

	private final AmazonS3Client amazonS3Client;

	@Value("${cloud.aws.s3.bucket}")
	private String bucket;

	public void uploadS3(MultipartFile multipartFile, String s3FileName) {
		ObjectMetadata objectMetaData = createObjectMetaData(multipartFile);

		try {
			amazonS3Client.putObject(bucket, s3FileName, multipartFile.getInputStream(), objectMetaData);
		} catch (IOException e) {
			throw new FileUploadFailureException(e.getMessage());
		}
	}

	public void uploadByteImageToS3(byte[] imageData, String s3FileName) {
		ObjectMetadata metadata = new ObjectMetadata();
		metadata.setContentType("image/jpeg"); // 또는 원하는 이미지 형식으로 변경
		metadata.setContentLength(imageData.length);

		ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(imageData);

		amazonS3Client.putObject(bucket, s3FileName, byteArrayInputStream, metadata);
	}

	private ObjectMetadata createObjectMetaData(final MultipartFile multipartFile) {
		ObjectMetadata objectMetadata = new ObjectMetadata();
		objectMetadata.setContentType(multipartFile.getContentType());
		objectMetadata.setContentLength(multipartFile.getSize());
		return objectMetadata;
	}
}
