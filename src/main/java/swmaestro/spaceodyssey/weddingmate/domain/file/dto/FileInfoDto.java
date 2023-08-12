package swmaestro.spaceodyssey.weddingmate.domain.file.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class FileInfoDto {
	String fileName;
	String fileExtension;

	@Builder
	public FileInfoDto(String fileName, String fileExtension) {
		this.fileName = fileName;
		this.fileExtension = fileExtension;
	}
}
