package swmaestro.spaceodyssey.weddingmate.domain.file.enums;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public enum FilePathType {
	PORTFOLIO("portfolio/"),
	PROFILE("profile/");

	private String path;

	FilePathType(String path) {
		this.path = path;
	}
}
