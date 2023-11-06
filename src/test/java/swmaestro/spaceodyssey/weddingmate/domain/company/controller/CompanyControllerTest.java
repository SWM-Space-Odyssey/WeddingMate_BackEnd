package swmaestro.spaceodyssey.weddingmate.domain.company.controller;

import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import javax.sound.sampled.Port;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.test.context.support.TestExecutionEvent;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import swmaestro.spaceodyssey.weddingmate.domain.company.entity.Companies;
import swmaestro.spaceodyssey.weddingmate.domain.company.repository.CompaniesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.file.repository.FilesRepository;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.domain.users.repository.UsersRepository;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;

@ActiveProfiles("test")
@Sql("classpath:db/teardown.sql")
@AutoConfigureMockMvc
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.MOCK)
public class CompanyControllerTest extends DummyEntity {

	@Autowired
	private MockMvc mockMvc;

	@Autowired
	private CompaniesRepository companiesRepository;

	@Autowired
	private UsersRepository usersRepository;

	@Autowired
	private PortfoliosRepository portfoliosRepository;

	@Autowired
	private ItemsRepository itemsRepository;

	@Autowired
	private FilesRepository filesRepository;

	Companies company;

	@BeforeEach
	public void setUp() {
		company = mockCompany();
		companiesRepository.save(company);

		Users user = newMockUser("test");
		usersRepository.save(user);

		Portfolios portfolio = newMockPortfolio(user);
		portfoliosRepository.save(portfolio);

		Items item = newMockItem(portfolio);
		item.setCompanies(company);
		itemsRepository.save(item);

		Files file = newMockFile();
		file.setItems(item);
		filesRepository.save(file);
	}

	@Test
	@DisplayName("업체 디테일")
	@WithUserDetails(value = "test@gmail.com", setupBefore = TestExecutionEvent.TEST_EXECUTION)
	void companyDetail() throws Exception {
		Long companyId = company.getCompanyId();

		ResultActions resultActions = mockMvc.perform(
			MockMvcRequestBuilders.get("/api/v1/company/{id}", companyId)
		).andDo(print());

		//then
		resultActions.andExpect(status().is2xxSuccessful());
	}
}
