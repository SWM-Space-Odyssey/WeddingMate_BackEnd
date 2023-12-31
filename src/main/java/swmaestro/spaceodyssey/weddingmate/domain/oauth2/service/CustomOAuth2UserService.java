package swmaestro.spaceodyssey.weddingmate.domain.oauth2.service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.OAuth2UserInfo;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.OAuth2UserInfoFactory;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.UserPrincipal;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.enums.AuthProvider;
import swmaestro.spaceodyssey.weddingmate.domain.profile.service.ProfilesFilesUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2AuthProviderIdNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.oauth2.OAuth2DuplicateEmailException;

import java.util.Optional;

import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.OAUTH_AGE_NOT_PROVIDED;
import static swmaestro.spaceodyssey.weddingmate.global.constant.ResponseConstant.OAUTH_GENDER_NOT_PROVIDED;

@Transactional
@RequiredArgsConstructor
@Service
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

	private final UsersRepository usersRepository;
	private final ProfilesFilesUploadService profilesFilesUploadService;

	@Value("${LOGIN_REDIRECT_URL}")
	private String loginRedirectUrl;

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

	@Transactional
	public Users registerUser(AuthProvider authProvider, OAuth2UserInfo oAuth2UserInfo) {
		try {
			Files profileImage = profilesFilesUploadService.createUserProfile(oAuth2UserInfo.getOAuth2Id(), oAuth2UserInfo.getImageUrl());

			boolean isTargetUser = checkOAuth2UserGenderAndAge(oAuth2UserInfo);

			Users users = Users.builder()
					.nickname(oAuth2UserInfo.getName())
					.email(oAuth2UserInfo.getEmail())
					.gender(oAuth2UserInfo.getGender())
					.age(oAuth2UserInfo.getAge())
					.authProvider(authProvider)
					.profileImage(profileImage)
					.authProviderId(oAuth2UserInfo.getOAuth2Id())
					.build();

			if (!isTargetUser) {
				users.setAccountStatusToNotEligible();
			}

			return usersRepository.save(users);
		} catch (DataIntegrityViolationException e) {
			// 이미 등록된 이메일 주소인 경우 DataIntegrityViolationException 발생
			throw new OAuth2DuplicateEmailException(authProvider);
		}
	}

	private Users updateUser(Users users, OAuth2UserInfo oAuth2UserInfo) {
		return usersRepository.save(users.updateOAuth2Info(oAuth2UserInfo));
	}

	private boolean checkOAuth2UserGenderAndAge(OAuth2UserInfo oAuth2UserInfo) {
		String gender = oAuth2UserInfo.getGender();
		String age = oAuth2UserInfo.getAge();

		return !gender.equals(OAUTH_GENDER_NOT_PROVIDED) && !age.equals(OAUTH_AGE_NOT_PROVIDED) && (age.equals("20~29") || age.equals("30~39"));
	}
}
