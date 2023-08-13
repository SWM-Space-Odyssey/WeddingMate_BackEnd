package swmaestro.spaceodyssey.weddingmate.domain.portfolio;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

public class PortfolioDummyEntity {

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

	public Portfolio newMockPortfolio(Users users) {
		Portfolio portfolio = Portfolio.builder()
			.title("Test Title")
			.users(users)
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
}
