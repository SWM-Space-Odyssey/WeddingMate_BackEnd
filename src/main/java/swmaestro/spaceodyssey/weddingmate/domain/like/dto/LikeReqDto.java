package swmaestro.spaceodyssey.weddingmate.domain.like.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class LikeReqDto {
	private Long id;
	private String likeType;
}