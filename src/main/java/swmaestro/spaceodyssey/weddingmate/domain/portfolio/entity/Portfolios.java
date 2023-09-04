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
import jakarta.validation.constraints.NotNull;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.global.entity.BaseTimeEntity;

@NoArgsConstructor
@Getter
@Entity
public class Portfolios extends BaseTimeEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long portfolioId;

	@Column(length =  50, nullable = false)
	private String title;

	@OneToMany(mappedBy = "portfolios", cascade = CascadeType.ALL)
	private final List<Items> portfolioItemsList = new ArrayList<>();

	@NotNull(message = "지역 태그는 빈칸일 수 없습니다")
	private String regionTag;

	@NotNull(message = "포트폴리오 태그는 빈칸일 수 없습니다")
	private String portfolioTagList;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "planner_id")
	private Planners planners;

	@Column(nullable = false)
	private Boolean isDeleted;

	@OneToOne(cascade = CascadeType.ALL)
	@JoinColumn(name = "file_id")
	private Files files;

	private Integer likeCount;

	public void updatePortfolio(String title, String regionTag, String portfolioTagList) {
		this.title = title;
		this.regionTag = regionTag;
		this.portfolioTagList = portfolioTagList;
	}

	public void deletePortfolio() {
		this.isDeleted = true;
	}

	@Builder
	public Portfolios(String title, Planners planners, String portfolioTagList, String regionTag) {
		this.title = title;
		this.planners = planners;
		this.portfolioTagList = portfolioTagList;
		this.regionTag = regionTag;
		this.isDeleted = false;
		this.likeCount = 0;
	}

	public void setFiles(Files files) {
		this.files = files;
	}

	public void setLikeCount(Integer likeCount) {
		this.likeCount = likeCount;
	}
}
