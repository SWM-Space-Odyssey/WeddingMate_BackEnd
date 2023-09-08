package swmaestro.spaceodyssey.weddingmate.global.config.test;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;


public class DummyEntity {

	public Users newMockUser(String nickname) {
		Files files = newMockFile();
		return Users.builder()
			.authProvider(AuthProvider.KAKAO)
			.authProviderId("test")
			.email(nickname + "@gmail.com")
			.nickname(nickname)
			.profileImage(files)
			.build();
	}

	public Planners newMockPlanner(Users users) {

		Planners planners = Planners.builder()
			.plannerTagList("친절한")
			.regionList("서울")
			.company("웨딩웨딩")
			.position("사원")
			.build();
		planners.setUsers(users);

		return planners;
	}

	public Portfolios newMockPortfolio(Planners planners) {
		Portfolios portfolios = Portfolios.builder()
			.title("Test Title")
			.planners(planners)
			.regionTag("서울")
			.portfolioTagList("예쁜,고급스러운")
			.build();

		portfolios.setFiles(newMockFile());

		return portfolios;
	}


	public Files newMockFile() {
		return swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files.builder()
			.url("/test")
			.filename("test")
			.filetype(".txt")
			.build();
	}

	public Items newMockItem(Portfolios portfolios) {
		return Items.builder()
			.portfolios(portfolios)
			.itemDate("localtime")
			.company("company")
			.itemOrder(1)
			.category("category")
			.itemTagList("tag1,tag2")
			.itemRecord("test")
			.build();
	}

	public UserLikes mockUserLike(Long likedId, LikeEnum likeEnum, Users users) {
		return UserLikes.builder()
			.likedId(likedId)
			.likeEnum(likeEnum)
			.users(users)
			.build();
	}
}
