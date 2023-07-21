package swmaestro.spaceodyssey.weddingmate.domain.oauth2;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.oauth2.core.user.OAuth2User;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;

@Getter
@AllArgsConstructor
public class UserPrincipal implements UserDetails, OAuth2User {

	private Long id;
	private String email;
	private Users users;
	private Collection<? extends GrantedAuthority> authorities;
	@Setter
	private transient Map<String, Object> attributes;

	public UserPrincipal(Long id, String email, Users users, Collection<? extends GrantedAuthority> authorities) {
		this.id = id;
		this.email = email;
		this.authorities = authorities;
		this.users = users;
	}

	public static UserPrincipal create(Users users) {
		List<GrantedAuthority> authorityList = Collections.singletonList(new SimpleGrantedAuthority("USER"));

		return new UserPrincipal(
			users.getUserId(),
			users.getEmail(),
			users,
			authorityList
		);
	}

	public static UserPrincipal create(Users users, Map<String, Object> attributes) {
		UserPrincipal userPrincipal = UserPrincipal.create(users);
		userPrincipal.setAttributes(attributes);
		return userPrincipal;
	}

	// OAuth2 Override
	@Override
	public String getName() {
		return String.valueOf(id);
	}

	@Override
	public Map<String, Object> getAttributes() {
		return attributes;
	}

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		return authorities;
	}

	// UserDetail Override
	@Override
	public String getPassword() {
		return null;
	}

	@Override
	public String getUsername() {
		return email;
	}

	@Override
	public boolean isAccountNonExpired() {
		return true;
	}

	@Override
	public boolean isAccountNonLocked() {
		return true;
	}

	@Override
	public boolean isCredentialsNonExpired() {
		return true;
	}

	@Override
	public boolean isEnabled() {
		return true;
	}
}
