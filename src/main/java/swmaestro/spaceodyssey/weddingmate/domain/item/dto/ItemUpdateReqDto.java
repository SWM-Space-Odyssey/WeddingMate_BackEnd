package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import java.util.List;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemUpdateReqDto {
	private String itemRecord;
	private String companyName;
	private String date;
	private String itemTagList;
	private String category;
	private List<String> imageList;
	private Long companyId;
}
