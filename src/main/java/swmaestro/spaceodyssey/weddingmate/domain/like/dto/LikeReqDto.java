package swmaestro.spaceodyssey.weddingmate.domain.like.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class LikeReqDto {
	private Long id;
	private String likeType;
}