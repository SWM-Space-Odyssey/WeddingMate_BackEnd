package swmaestro.spaceodyssey.weddingmate.domain.chat.controller;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Hidden;
import lombok.RequiredArgsConstructor;
import swmaestro.spaceodyssey.weddingmate.domain.chat.dto.ChatMessageReqDto;
import swmaestro.spaceodyssey.weddingmate.domain.chat.service.ChatRoomsService;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.AuthUsers;
import swmaestro.spaceodyssey.weddingmate.domain.users.entity.Users;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponse;
import swmaestro.spaceodyssey.weddingmate.global.dto.ApiResponseStatus;

@Hidden
@RequiredArgsConstructor
@RestController
@RequestMapping("/api/v1/chat")
public class ChatRoomsController {

	private final ChatRoomsService chatRoomService;

	@PostMapping("/room")
	public ApiResponse<Object> createRoom(@AuthUsers Users users, @RequestBody ChatMessageReqDto reqDto) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(chatRoomService.createRoom(users, reqDto))
			.build();
	}

	@GetMapping("/rooms")
	public ApiResponse<Object> findAllRoomByUser(@AuthUsers Users users) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(chatRoomService.findAllRoomByUser(users))
			.build();
	}

	@GetMapping("/room/{roomId}")
	public ApiResponse<Object> findRoomByRoomId(@PathVariable String roomId) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(chatRoomService.findRoomByRoomId(roomId))
			.build();
	}

	@DeleteMapping("/room/{roomId}")
	public ApiResponse<Object> deleteRoom(@AuthUsers Users users, @PathVariable String roomId) {
		return ApiResponse.builder()
			.status(ApiResponseStatus.SUCCESS)
			.data(chatRoomService.deleteRoom(users, roomId))
			.build();
	}
}
