package swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.tag.entity.Tag;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Customer;

@Entity
@Getter
@NoArgsConstructor
public class PortfolioTag {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@ManyToOne
	@JoinColumn
	private Portfolio portfolio;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn
	private Customer customer;

	@ManyToOne
	@JoinColumn
	private Tag tag;

	@Builder(builderMethodName = "portfolioBuilder")
	public PortfolioTag(Portfolio portfolio, Tag tag) {
		this.portfolio = portfolio;
		this.tag = tag;
	}

	@Builder(builderMethodName = "customerBuilder")
	public PortfolioTag(Customer customer, Tag tag) {
		this.customer = customer;
		this.tag = tag;
	}
}
