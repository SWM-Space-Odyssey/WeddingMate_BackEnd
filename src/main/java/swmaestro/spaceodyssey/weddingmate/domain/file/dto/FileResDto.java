package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class FileResDto {
	private String uplaodFileName;
	private String uploadFileUrl;

	@Builder
	public FileResDto(String uplaodFileName, String uploadFileUrl) {
		this.uplaodFileName = uplaodFileName;
		this.uploadFileUrl = uploadFileUrl;
	}
}
