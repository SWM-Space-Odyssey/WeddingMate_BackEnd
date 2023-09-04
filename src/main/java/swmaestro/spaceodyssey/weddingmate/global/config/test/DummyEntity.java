package swmaestro.spaceodyssey.weddingmate.global.config.test;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLike;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planner;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;


public class DummyEntity {

	public Users newMockUser(String nickname) {
		File file = newMockFile();
		return Users.builder()
			.authProvider(AuthProvider.KAKAO)
			.authProviderId("test")
			.email(nickname + "@gmail.com")
			.nickname(nickname)
			.profileImage(file)
			.build();
	}

	public Planner newMockPlanner(Users users) {

		Planner planner = Planner.builder()
			.plannerTagList("친절한")
			.regionList("서울")
			.company("웨딩웨딩")
			.position("사원")
			.build();
		planner.setUsers(users);

		return planner;
	}

	public Portfolio newMockPortfolio(Planner planner) {
		Portfolio portfolio = Portfolio.builder()
			.title("Test Title")
			.planner(planner)
			.regionTag("서울")
			.portfolioTagList("예쁜,고급스러운")
			.build();

		portfolio.setFile(newMockFile());

		return portfolio;
	}


	public File newMockFile() {
		return File.builder()
			.url("/test")
			.filename("test")
			.filetype(".txt")
			.build();
	}

	public Item newMockItem(Portfolio portfolio) {
		return Item.builder()
			.portfolio(portfolio)
			.itemDate("localtime")
			.company("company")
			.itemOrder(1)
			.category("category")
			.itemTagList("tag1,tag2")
			.itemRecord("test")
			.build();
	}

	public UserLike mockUserLike(Long likedId, LikeEnum likeEnum, Users users) {
		return UserLike.builder()
			.likedId(likedId)
			.likeEnum(likeEnum)
			.users(users)
			.build();
	}
}
