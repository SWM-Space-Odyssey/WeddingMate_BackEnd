package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class FileInfoDto {
	private String fileName;
	private String fileExtension;

	@Builder
	public FileInfoDto(String fileName, String fileExtension) {
		this.fileName = fileName;
		this.fileExtension = fileExtension;
	}
}
