package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class ItemUpdateReqDto {
	private String itemRecord;
	private String company;
	private String date;
	private String itemTagList;
	private String category;
	private List<String> imageList;
}
