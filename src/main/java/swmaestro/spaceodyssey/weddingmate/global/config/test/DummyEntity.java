package swmaestro.spaceodyssey.weddingmate.global.config.test;

import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.like.entity.UserLikes;
import swmaestro.spaceodyssey.weddingmate.domain.like.enums.LikeEnum;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.report.entity.Report;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportItemType;
import swmaestro.spaceodyssey.weddingmate.domain.report.enums.ReportReason;
import swmaestro.spaceodyssey.weddingmate.domain.users.dto.CustomerTagListDto;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customers;
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
			.age("20")
			.gender("여성")
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

	public Customers newMockCustomer(Users users) {

		CustomerTagListDto customerTagListDto = CustomerTagListDto.builder()
				.portfolioTagList("PortfolioTag1, PortfolioTag2")
				.plannerTagList("PlannerTag1, PlannerTag2")
				.dressTagList("DressTag1, DressTag2")
				.studioTypeTagList("StudioTypeTag1, StudioTypeTag2")
				.studioFocusTagList("StudioFocusTag1, StudioFocusTag2")
				.makeupTagList("MakeupTag1, MakeupTag2")
				.build();

		Customers customers = Customers.builder()
				.customerTagList(customerTagListDto)
				.budget("저렴한")
				.weddingDateConfirmed(true)
				.regionList("서울")
				.build();

		customers.setUsers(users);

		return customers;
	}

	public Portfolios newMockPortfolio(Users users) {
		Portfolios portfolios = Portfolios.builder()
				.title("Test Title")
				.users(users)
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

	public Report newMockReport(Users reporterUser, Users reportedUser, ReportItemType reportItemType, Long reportItemId) {
		return Report.builder()
				.reporterUser(reporterUser)
				.reportedUser(reportedUser)
				.reportReason(ReportReason.SPAM)
				.reportItemType(reportItemType)
				.reportItemId(reportItemId)
				.build();
	}

	public Companies mockCompany() {
		return Companies.builder()
			.name("test")
			.address("testAddress")
			.category("드레스")
			.build();
	}
}
