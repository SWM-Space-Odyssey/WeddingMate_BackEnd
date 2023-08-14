package swmaestro.spaceodyssey.weddingmate.domain.item.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ItemFileReqDto {
	Long portfolioId;
	String itemCategory;
}
