package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Pattern;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.OAuth2UserInfo;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserAccountStatusEnum;
import swmaestro.spaceodyssey.weddingmate.domain.users.enums.UserRegisterStatusEnum;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@SuppressWarnings("checkstyle:RegexpMultiline")
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Entity
public class Users extends BaseTimeEntity {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long userId;

	@Email(message = "올바르지 않은 이메일 형식입니다.",
		regexp = "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}$")
	@Column(nullable = false)
	private String email;

	private String nickname;

	@Pattern(regexp = "^[0-9]{3}-[0-9]{4}-[0-9]{4}$", message = "올바르지 않은 핸드폰 번호 형식입니다.")
	private String phone;

	@OneToOne(cascade = CascadeType.ALL)
	@JoinColumn(name = "profileImage_id")
	private File profileImage;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private UserRegisterStatusEnum registerStatus;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private UserAccountStatusEnum accountStatus;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private AuthProvider authProvider;

	private String authProviderId;

	@Column(nullable = false)
	private String role;

	@Column(nullable = false)
	private Integer blockCnt = 0;

	@Column(nullable = false)
	private Integer reportCnt = 0;

	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@JoinColumn(name = "planner_id")
	private Planner planner;

	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@JoinColumn(name = "customer_id")
	private Customer customer;

	@OneToMany(mappedBy = "users", cascade = CascadeType.ALL, orphanRemoval = true)
	private List<Portfolio> portfolioList;

	@Builder
	public Users(String email, String nickname, File profileImage, AuthProvider authProvider, String authProviderId) {
		this.email = email;
		this.nickname = nickname;
		this.profileImage = profileImage;
		this.authProvider = authProvider;
		this.authProviderId = authProviderId;
		this.registerStatus = UserRegisterStatusEnum.UNREGISTERED;
		this.accountStatus = UserAccountStatusEnum.NORMAL;
		this.role = "USER";
		this.blockCnt = 0;
		this.reportCnt = 0;
	}

	public Users updateOAuth2Info(OAuth2UserInfo oAuth2UserInfo) {
		this.email = oAuth2UserInfo.getEmail();
		this.authProviderId = oAuth2UserInfo.getOAuth2Id();

		return this;
	}

	public void updateRegisterStatus(UserRegisterStatusEnum registerStatus) {
		this.registerStatus = registerStatus;
	}

	public void updateNickname(String nickname) {
		this.nickname = nickname;
	}

	public void setPlanner(Planner planner) {
		this.planner = planner;

		if (planner.getUsers() != this) {
			planner.setUsers(this);
		}
	}

	public void setCustomer(Customer customer) {
		this.customer = customer;

		if (customer.getUsers() != this) {
			customer.setUsers(this);
		}
	}

	public void createPhone(String phone) {
		this.phone = phone;
	}

	public void updateProfileImage(File profileImage) {
		this.profileImage = profileImage;
	}
}
