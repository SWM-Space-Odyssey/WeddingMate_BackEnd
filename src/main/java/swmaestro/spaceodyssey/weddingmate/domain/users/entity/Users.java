package swmaestro.spaceodyssey.weddingmate.domain.users.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Pattern;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.OAuth2UserInfo;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@SuppressWarnings("checkstyle:RegexpMultiline")
@NoArgsConstructor
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

	private String imageUrl;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private UserEnum state;

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

	@Builder
	public Users(String email, String nickname, String imageUrl, AuthProvider authProvider, String authProviderId) {
		this.email = email;
		this.nickname = nickname;
		this.imageUrl = imageUrl;
		this.authProvider = authProvider;
		this.authProviderId = authProviderId;
		this.state = UserEnum.NORMAL;
		this.role = "USER";
		this.blockCnt = 0;
		this.reportCnt = 0;
	}

	public Users updateOAuth2Info(OAuth2UserInfo oAuth2UserInfo) {
		this.email = oAuth2UserInfo.getEmail();
		this.authProviderId = oAuth2UserInfo.getOAuth2Id();

		return this;
	}
}
