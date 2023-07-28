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
import jakarta.persistence.OneToOne;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
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

	@OneToMany(mappedBy = "portfolio", cascade = CascadeType.PERSIST)
	private List<PortfolioTag> portfolioTagList = new ArrayList<>();

	@OneToMany(mappedBy = "portfolio", cascade = CascadeType.ALL)
	private final List<Item> portfolioItemList = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id")
	private Users users;

	@Column(nullable = false)
	private Boolean isDeleted;

	@OneToOne(cascade = CascadeType.ALL)
	@JoinColumn(name = "file_id")
	private File file;

	public void updatePortfolio(String title, List<PortfolioTag> portfolioTagList) {
		this.title = title;
		this.portfolioTagList = portfolioTagList;
	}

	public void deletePortfolio() {
		this.isDeleted = true;
	}

	@Builder
	public Portfolio(String title, String repImageUrl, Users users, List<PortfolioTag> portfolioTagList) {
		this.title = title;
		this.users = users;
		this.portfolioTagList = portfolioTagList;
		this.isDeleted = false;
	}

	public void setPortfolioTag(List<PortfolioTag> portfolioTagList) {
		this.portfolioTagList = portfolioTagList;
	}

	public void setFile(File file) {
		this.file = file;
	}
}
