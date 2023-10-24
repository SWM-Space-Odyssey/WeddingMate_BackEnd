package swmaestro.spaceodyssey.weddingmate.domain.portfolio.service;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.Files;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemOrderDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Items;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemsRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolios;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Planners;
import swmaestro.spaceodyssey.weddingmate.global.config.test.DummyEntity;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper.PortfoliosMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfoliosRepository;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.exception.portfolio.PortfolioNotFoundException;
import swmaestro.spaceodyssey.weddingmate.global.exception.users.UserUnAuthorizedException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

//@DisplayName()
@ExtendWith(MockitoExtension.class)
public class PortfoliosServiceTest extends DummyEntity {

	@InjectMocks
	private PortfolioService portfolioService;

	@Mock
	private PortfolioFileUploadService portfolioFileUploadService;

	@Mock //진짜 객체 주입
	private PortfoliosMapper portfoliosMapper;

	@Mock
	private PortfoliosRepository portfoliosRepository;

	@Mock
	private ItemsRepository itemsRepository;

	public Users mockUser;

	public Planners mockPlanners;

	public Portfolios mockPortfolios;
	public Files mockFiles;

	@BeforeEach
	void setup() {
		mockUser = newMockUser("testUser");
		ReflectionTestUtils.setField(mockUser, "userId", 1L);
		mockPortfolios = newMockPortfolio(mockUser);
		ReflectionTestUtils.setField(mockPortfolios, "portfolioId", 1L);
		mockFiles = newMockFile();
	}

	@Test
	@DisplayName("포트폴리오 생성 성공")
	public void testCreatePortfolio() throws IOException {
		MultipartFile mockMultipartFile = mock(MockMultipartFile.class);
		PortfolioSaveReqDto mockReqDto = new PortfolioSaveReqDto("test", "tag1,tag2","서울");

		//stub
		when(portfoliosMapper.dtoToEntity(any(), any())).thenReturn(mockPortfolios);
		when(portfolioFileUploadService.uploadPortfolioFile(any(), any())).thenReturn(mockFiles);
		when(portfoliosRepository.save(any())).thenReturn(mockPortfolios);

		// When
		Long portfolioId = portfolioService.createPortfolio(mockUser, mockMultipartFile, mockReqDto);

		// Verify method calls
		verify(portfoliosMapper, times(1)).dtoToEntity(any(), any());
		verify(portfolioFileUploadService, times(1)).uploadPortfolioFile(any(), any());
		verify(portfoliosRepository, times(1)).save(any());

		// Then
		Assertions.assertThat(portfolioId).isEqualTo(mockPortfolios.getPortfolioId());
		Assertions.assertThat(mockPortfolios.getPortfolioTagList()).isEqualTo("예쁜,고급스러운");
		Assertions.assertThat(mockPortfolios.getTitle()).isEqualTo("Test Title");
	}

	@Test
	@DisplayName("포트폴리오 업데이트 성공")
	public void testUpdatePortfolio() throws IOException {
		MultipartFile mockMultipartFile = mock(MockMultipartFile.class);
		List<ItemOrderDto> mockItemDtoList = new ArrayList<>();
		PortfolioUpdateReqDto mockUpdateReqDto = new PortfolioUpdateReqDto("아름다운 결혼식", "서울", "고급스러운", mockItemDtoList);
		Long portfolioId = 1L;
		//ReflectionTestUtils.setField(mockUpdateReqDto, "itemOrderList", mockItemDtoList);

		//stub
		when(portfoliosRepository.findById(any())).thenReturn(java.util.Optional.of(mockPortfolios));
		when(portfolioFileUploadService.uploadPortfolioFile(any(), any())).thenReturn(mockFiles);

		//When
		Long updatedPortfolioId = portfolioService.updatePortfolio(mockUser, portfolioId, mockUpdateReqDto, mockMultipartFile);

		//Verify
		verify(portfoliosRepository, times(1)).findById(portfolioId);
		verify(portfolioFileUploadService, times(1)).uploadPortfolioFile(any(), any());

		//Then
		Assertions.assertThat(updatedPortfolioId).isEqualTo(mockPortfolios.getPortfolioId());
	}

	@Test
	@DisplayName("프토플리오 업데이트 실패")
	public void testUpdatePortfolio_WithUnAuthorizedUser() throws IOException {
		Users newMockUser = newMockUser("new user");
		ReflectionTestUtils.setField(newMockUser, "userId", 2L);
		List<ItemOrderDto> mockItemDtoList = new ArrayList<>();
		PortfolioUpdateReqDto mockUpdateReqDto = new PortfolioUpdateReqDto("아름다운 결혼식", "서울", "고급스러운", mockItemDtoList);
		Long portfolioId = 1L;
		MultipartFile mockMultipartFile = mock(MockMultipartFile.class);

		//stub
		when(portfoliosRepository.findById(any())).thenReturn(java.util.Optional.of(mockPortfolios));

		//When
		assertThrows(UserUnAuthorizedException.class, () -> portfolioService.updatePortfolio(newMockUser, portfolioId, mockUpdateReqDto, mockMultipartFile));
	}

	@Test
	@DisplayName("프토플리오 업데이트 실패")
	public void testUpdatePortfolio_PortfolioDeleted() throws IOException {
		List<ItemOrderDto> mockItemDtoList = new ArrayList<>();
		PortfolioUpdateReqDto mockUpdateReqDto = new PortfolioUpdateReqDto("아름다운 결혼식", "서울", "고급스러운", mockItemDtoList);
		Long portfolioId = 1L;
		MultipartFile mockMultipartFile = mock(MockMultipartFile.class);
		ReflectionTestUtils.setField(mockPortfolios, "isDeleted", true);

		//stub
		when(portfoliosRepository.findById(any())).thenReturn(java.util.Optional.of(mockPortfolios));

		//When
		assertThrows(PortfolioNotFoundException.class, () -> portfolioService.updatePortfolio(mockUser, portfolioId, mockUpdateReqDto, mockMultipartFile));
	}

	@Test
	@DisplayName("포트폴리오 삭제 성공")
	public void testDeletePortfolio() {
		Items mockItems1 = newMockItem(mockPortfolios);
		Items mockItems2 = newMockItem(mockPortfolios);
		List<Items> mockItemsList = Arrays.asList(mockItems1, mockItems2);
		ReflectionTestUtils.setField(mockPortfolios, "portfolioItemList", mockItemsList);

		//stub
		when(portfoliosRepository.findById(any())).thenReturn(java.util.Optional.of(mockPortfolios));

		//then
		portfolioService.deletePortfolio(mockUser, mockPortfolios.getPortfolioId());

		// Verify
		verify(portfoliosRepository, times(1)).findById(any());

		// Perform assertions
		Assertions.assertThat(mockPortfolios.getIsDeleted()).isTrue();
		Assertions.assertThat(mockItems1.getIsDeleted()).isTrue();
		Assertions.assertThat(mockItems2.getIsDeleted()).isTrue();
	}



}
