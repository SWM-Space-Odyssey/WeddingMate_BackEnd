package swmaestro.spaceodyssey.weddingmate.domain.like.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.company.service.CompaniesRepositoryService;
import swmaestro.spaceodyssey.weddingmate.domain.like.dto.CompanyLikeResDto;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.like.mapper.LikesMapper;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.config.aop.distributedLock.DistributedLock;

import java.util.List;

@Component
@RequiredArgsConstructor
public class CompanyLikeImpl implements LikesService {
	private final LikesRepositoryService likesRepositoryService;
	private final CompaniesRepositoryService companiesRepositoryService;
	private final LikesMapper likesMapper;

	@Override
	@DistributedLock(key = "#lockName")
	public void increaseLikeCount(String lockName, Long id) {

		Companies companies = companiesRepositoryService.findCompanyById(id);
		companies.increaseLikeCount();
	}

	@Override
	@DistributedLock(key = "#lockName")
	public void decreaseLikeCount(String lockName, Long id) {

		Companies companies = companiesRepositoryService.findCompanyById(id);
		companies.decreaseLikeCount();
	}

	@Override
	@Transactional(readOnly = true)
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