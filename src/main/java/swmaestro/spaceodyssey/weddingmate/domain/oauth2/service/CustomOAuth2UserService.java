package swmaestro.spaceodyssey.weddingmate.domain.oauth2.service;

import java.util.Optional;

import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.OAuth2UserInfo;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.OAuth2UserInfoFactory;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.UserPrincipal;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2AuthProviderIdNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2DuplicateEmailException;

@Transactional
@RequiredArgsConstructor
@Service
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

	private final UsersRepository usersRepository;

	/* OAuth2UserRequest에 이는 Access Token으로 유저 정보를 가져온다 */
	@Override
	public OAuth2User loadUser(OAuth2UserRequest userRequest) throws OAuth2AuthenticationException {

		OAuth2UserService<OAuth2UserRequest, OAuth2User> service = new DefaultOAuth2UserService();
		OAuth2User oAuth2User = service.loadUser(userRequest);

		return processOAuth2User(userRequest, oAuth2User);
	}

	/* 획득한 유저 정보를 Java model과 mapping하고 process 진행 */
	protected OAuth2User processOAuth2User(OAuth2UserRequest userRequest, OAuth2User oAuth2User) {
		// 로그인 플랫폼 확인(원래는 enum으로 하지만 우리는 kakao만 함)
		AuthProvider authProvider = AuthProvider.valueOf(
			userRequest.getClientRegistration().getRegistrationId().toUpperCase());

		OAuth2UserInfo oAuth2UserInfo = OAuth2UserInfoFactory.getOAuth2UserInfo(authProvider,
			oAuth2User.getAttributes());

		if (!StringUtils.hasLength(oAuth2UserInfo.getOAuth2Id())) {
			throw new OAuth2AuthProviderIdNotFoundException();
		}

		Optional<Users> usersOptional = usersRepository.findByAuthProviderId(oAuth2UserInfo.getOAuth2Id());
		Users user = usersOptional.map(existingUser -> {
			// 가져온 유저의 provider명과 넘어온 provider명이 다른 경우(ex. kakao vs naver)
			if (!existingUser.getAuthProvider().equals(authProvider)) {
				// 이미 다른 provider로 가입되어 있기 때문에 가입 불가
				throw new OAuth2DuplicateEmailException(existingUser.getAuthProvider());
			}
			return updateUser(existingUser, oAuth2UserInfo);
		}).orElseGet(() -> registerUser(authProvider, oAuth2UserInfo));

		return UserPrincipal.create(user, oAuth2UserInfo.getAttributes());
	}

	private Users registerUser(AuthProvider authProvider, OAuth2UserInfo oAuth2UserInfo) {
		Users users = Users.builder()
			.nickname(oAuth2UserInfo.getName())
			.email(oAuth2UserInfo.getEmail())
			.imageUrl(oAuth2UserInfo.getImageUrl())
			.authProvider(authProvider)
			.authProviderId(oAuth2UserInfo.getOAuth2Id())
			.build();

		return usersRepository.save(users);
	}

	private Users updateUser(Users users, OAuth2UserInfo oAuth2UserInfo) {
		return usersRepository.save(users.updateOAuth2Info(oAuth2UserInfo));
	}
}
