package swmaestro.spaceodyssey.weddingmate.domain.file.service;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import java.io.IOException;

import org.apache.tika.Tika;
import org.springframework.web.multipart.MultipartFile;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNameEmptyException;
import swmaestro.spaceodyssey.weddingmate.global.exception.file.FileNotImageException;

@NoArgsConstructor(access= AccessLevel.PRIVATE)
public class ImageFilesValidator {

	// Tike : 이미지 파일인지 확인해주는 library
	private static final Tika tika = new Tika();

	public static void checkEmtpyFileName(final String fileName) {
		if (fileName == null || fileName.isBlank()) {
			throw new FileNameEmptyException();
		}
	}

	public static void checkImageType(final MultipartFile uploadImageFile) {
		try {
			final String mimeType = tika.detect(uploadImageFile.getInputStream());
			if (!mimeType.startsWith("image")) {
				throw new FileNotImageException(mimeType);
			}
		} catch (IOException e) {
			throw new FileNotImageException(FILE_IMAGE_READ_FAILURE + e);
		}
	}

}
