package swmaestro.spaceodyssey.weddingmate.global.exception.handler;

import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
public class GlobalExceptionHandler {

	// TODO : EXAMPLE 추후 삭제
	// vaild 오류
//	@ExceptionHandler(MethodArgumentNotValidException.class)
//	public ResponseEntity<Map<String, String>> handleValidationExceptions(MethodArgumentNotValidException ex){
//		Map<String, String> errors = new HashMap<>();
//		ex.getBindingResult().getAllErrors()
//				.forEach(c -> errors.put(((FieldError) c).getField(), c.getDefaultMessage()));
//		return ResponseEntity.badRequest().body(errors);
//	}
//
//	/*================== Diary Exception ==================*/
//	@ExceptionHandler(DuplicateDiaryException.class)
//	protected final ResponseEntity<ErrorResponse> handleDuplicateDiaryException(DuplicateDiaryException e) {
//		return ErrorResponse.toErrorResponseEntity(ErrorCode.DUPLICATE_DIARY_DATE, e.getMessage());
//	}


}
