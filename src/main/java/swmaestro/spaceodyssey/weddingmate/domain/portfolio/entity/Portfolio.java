package swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity;

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor
@Getter
@Entity
public class Portfolio extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long portfolioId;

	@Column(length =  50, nullable = false)
	private String title;

	@Column(nullable = true)
	private String repImageUrl;

	@OneToMany(mappedBy = "portfolio", cascade = CascadeType.ALL, orphanRemoval = true)
	private List<PortfolioTag> portfolioTagList = new ArrayList<>();

	@OneToMany(mappedBy = "portfolio", cascade = CascadeType.ALL)
	private final List<Item> portfolioItemList = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id")
	private Users users;

	@Builder
	public Portfolio(String title, String repImageUrl, Users users, List<PortfolioTag> portfolioTagList) {
		this.title = title;
		this.repImageUrl = repImageUrl;
		this.users = users;
		this.portfolioTagList = portfolioTagList;
	}

	public void setPortfolioTag(List<PortfolioTag> portfolioTagList) {
		this.portfolioTagList = portfolioTagList;
	}
}
