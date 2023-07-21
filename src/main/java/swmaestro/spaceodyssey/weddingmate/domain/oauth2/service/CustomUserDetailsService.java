package swmaestro.spaceodyssey.weddingmate.domain.oauth2.service;

import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import swmaestro.spaceodyssey.weddingmate.domain.oauth2.UserPrincipal;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNameNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserNotFoundException;

// Spring Security에서 유저의 정보를 가져오는 클래스
@Service
public class CustomUserDetailsService implements UserDetailsService {

	private final UsersRepository usersRepository;

	public CustomUserDetailsService(UsersRepository usersRepository) {
		this.usersRepository = usersRepository;
	}

	@Override
	@Transactional
	public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
		Users users = usersRepository.findByEmail(email)
			.orElseThrow(UserNameNotFoundException::new);
		return UserPrincipal.create(users);
	}

	@Transactional
	public UserDetails loadUserById(Long id) {
		Users users = usersRepository.findById(id)
			.orElseThrow(UserNotFoundException::new);

		return UserPrincipal.create(users);
	}
}
