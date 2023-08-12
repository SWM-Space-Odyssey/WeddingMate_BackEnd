package swmaestro.spaceodyssey.weddingmate.global.exception.file;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.*;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
public class FileKakaoProfileDownloadFailureException extends RuntimeException {
	public FileKakaoProfileDownloadFailureException() {
		super(FILE_KAKAO_PROFILE_DOWNLOAD_FAILURE);
	}

	public FileKakaoProfileDownloadFailureException(String message) {
		super(FILE_KAKAO_PROFILE_DOWNLOAD_FAILURE + message);
	}
}
