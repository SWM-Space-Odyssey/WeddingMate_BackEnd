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

import swmaestro.spaceodyssey.weddingmate.domain.file.entity.File;
import swmaestro.spaceodyssey.weddingmate.domain.file.service.FileUploadService;
import swmaestro.spaceodyssey.weddingmate.domain.item.dto.ItemOrderDto;
import swmaestro.spaceodyssey.weddingmate.domain.item.entity.Item;
import swmaestro.spaceodyssey.weddingmate.domain.item.repository.ItemRepository;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.PortfolioDummyEntity;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.mapper.PortfolioMapper;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioSaveReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.dto.PortfolioUpdateReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.entity.Portfolio;
import swmaestro.spaceodyssey.weddingmate.domain.portfolio.repository.PortfolioRepository;
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
public class PortfolioServiceTest extends PortfolioDummyEntity {

	@InjectMocks
	private PortfolioService portfolioService;

	@Mock
	private FileUploadService fileUploadService;

	@Mock //진짜 객체 주입
	private PortfolioMapper portfolioMapper;

	@Mock
	private PortfolioRepository portfolioRepository;

	@Mock
	private ItemRepository itemRepository;

	public Users mockUser;
	public Portfolio mockPortfolio;
	public File mockFile;

	@BeforeEach
	void setup() {
		mockUser = newMockUser("testUser");
		ReflectionTestUtils.setField(mockUser, "userId", 1L);
		mockPortfolio = newMockPortfolio(mockUser);
		ReflectionTestUtils.setField(mockPortfolio, "portfolioId", 1L);
		mockFile = newMockFile();
	}

	@Test
	@DisplayName("포트폴리오 생성 성공")
	public void testCreatePortfolio() throws IOException {
		MultipartFile mockMultipartFile = mock(MockMultipartFile.class);
		PortfolioSaveReqDto mockReqDto = new PortfolioSaveReqDto("test", "tag1,tag2","서울");

		//stub
		when(portfolioMapper.dtoToEntity(any(), any())).thenReturn(mockPortfolio);
		when(fileUploadService.uploadPortfolioFile(any(), any())).thenReturn(mockFile);
		when(portfolioRepository.save(any())).thenReturn(mockPortfolio);

		// When
		Long portfolioId = portfolioService.createPortfolio(mockUser, mockMultipartFile, mockReqDto);

		// Verify method calls
		verify(portfolioMapper, times(1)).dtoToEntity(any(), any());
		verify(fileUploadService, times(1)).uploadPortfolioFile(any(), any());
		verify(portfolioRepository, times(1)).save(any());

		// Then
		Assertions.assertThat(portfolioId).isEqualTo(mockPortfolio.getPortfolioId());
		Assertions.assertThat(mockPortfolio.getPortfolioTagList()).isEqualTo("예쁜,고급스러운");
		Assertions.assertThat(mockPortfolio.getTitle()).isEqualTo("Test Title");
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
		when(portfolioRepository.findById(any())).thenReturn(java.util.Optional.of(mockPortfolio));
		when(fileUploadService.uploadPortfolioFile(any(), any())).thenReturn(mockFile);

		//When
		Long updatedPortfolioId = portfolioService.updatePortfolio(mockUser, portfolioId, mockUpdateReqDto, mockMultipartFile);

		//Verify
		verify(portfolioRepository, times(1)).findById(portfolioId);
		verify(fileUploadService, times(1)).uploadPortfolioFile(any(), any());

		//Then
		Assertions.assertThat(updatedPortfolioId).isEqualTo(mockPortfolio.getPortfolioId());
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
		when(portfolioRepository.findById(any())).thenReturn(java.util.Optional.of(mockPortfolio));

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
		ReflectionTestUtils.setField(mockPortfolio, "isDeleted", true);

		//stub
		when(portfolioRepository.findById(any())).thenReturn(java.util.Optional.of(mockPortfolio));

		//When
		assertThrows(PortfolioNotFoundException.class, () -> portfolioService.updatePortfolio(mockUser, portfolioId, mockUpdateReqDto, mockMultipartFile));
	}

	@Test
	@DisplayName("포트폴리오 삭제 성공")
	public void testDeletePortfolio() {
		Item mockItem1 = newMockItem(mockPortfolio);
		Item mockItem2 = newMockItem(mockPortfolio);
		List<Item> mockItemList = Arrays.asList(mockItem1, mockItem2);
		ReflectionTestUtils.setField(mockPortfolio, "portfolioItemList", mockItemList);

		//stub
		when(portfolioRepository.findById(any())).thenReturn(java.util.Optional.of(mockPortfolio));

		//then
		portfolioService.deletePortfolio(mockUser, mockPortfolio.getPortfolioId());

		// Verify
		verify(portfolioRepository, times(1)).findById(any());

		// Perform assertions
		Assertions.assertThat(mockPortfolio.getIsDeleted()).isTrue();
		Assertions.assertThat(mockItem1.getIsDeleted()).isTrue();
		Assertions.assertThat(mockItem2.getIsDeleted()).isTrue();
	}



}
