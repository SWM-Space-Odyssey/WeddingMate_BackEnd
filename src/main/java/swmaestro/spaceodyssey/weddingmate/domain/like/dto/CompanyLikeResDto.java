package swmaestro.spaceodyssey.weddingmate.domain.like.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class CompanyLikeResDto {
	private Long id;
	private String name;
	private String address;
	//private String repImgUrl;

	@Builder
	public CompanyLikeResDto(Long id, String name, String address, String repImgUrl) {
		this.id = id;
		this.name = name;
		this.address = address;
		//this.repImgUrl = repImgUrl;
	}
}
