package swmaestro.spaceodyssey.weddingmate.domain.company.dto;

import java.util.List;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class CompanyResDto {
	private String name;
	private String address;
	private List<CompanyItemResDto> itemList;
	private Boolean isMoreThanNineImages;
	private List<String> imageList;

	@Builder
	public CompanyResDto(String name, String address, List<CompanyItemResDto> itemList, List<String> imageList, Boolean isMoreThanNineImages) {
		this.name = name;
		this.address = address;
		this.itemList = itemList;
		this.imageList = imageList;
		this.isMoreThanNineImages = isMoreThanNineImages;
	}
}
