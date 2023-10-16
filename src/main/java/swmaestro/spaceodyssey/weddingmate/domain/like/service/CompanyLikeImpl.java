package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import java.util.List;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.company.service.CompaniesRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.CompanyLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.config.aop.DistributedLock.DistributedLock;

@Component
@RequiredArgsConstructor
public class CompanyLikeImpl implements LikeService {
	private final LikesRepositoryService likesRepositoryService;
	private final CompaniesRepositoryService companiesRepositoryService;
	private final LikesMapper likesMapper;

	@Override
	@DistributedLock(key="#lockName")
	public void updateLikeCount(String lockName, Long id, boolean isIncrement) {

		Companies companies = companiesRepositoryService.findCompanyById(id);

		if (isIncrement) {
			companies.setLikeCount(companies.getLikeCount() + 1);
		} else {
			companies.setLikeCount(companies.getLikeCount() - 1);
		}
	}

	@Override
	public List<CompanyLikeResDto> getUserLiked(Users users) {

		List<UserLikes> likeList = likesRepositoryService.getLikesByUsersAndType(users, LikeEnum.company);

		return likeList.stream()
			.map(userLikes -> {
				Long companyId = userLikes.getLikedId();
				Companies company = companiesRepositoryService.findCompanyById(companyId);
				return likesMapper.entityToDto(company);
			})
			.toList();
	}
}
