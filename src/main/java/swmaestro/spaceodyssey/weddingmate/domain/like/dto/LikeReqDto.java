package swmaestro.spaceodyssey.weddingmate.domain.like.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class LikeReqDto {
	private Long id;
	private LikeEnum likeType;
}