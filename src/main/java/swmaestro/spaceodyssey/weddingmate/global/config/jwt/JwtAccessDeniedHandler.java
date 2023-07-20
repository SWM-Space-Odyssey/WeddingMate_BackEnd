package swmaestro.spaceodyssey.weddingmate.global.config.jwt;

import java.io.IOException;

import org.json.JSONObject;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.stereotype.Component;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@Component
public class JwtAccessDeniedHandler implements AccessDeniedHandler {
	@Override
	public void handle(HttpServletRequest request, HttpServletResponse response,
		AccessDeniedException accessDeniedException) throws IOException, ServletException {
		response.setContentType("application/json");
		response.setCharacterEncoding("utf-8");
		response.setStatus(HttpServletResponse.SC_FORBIDDEN);
		response.getWriter().write(new JSONObject()
			.put("message", accessDeniedException.getMessage()).toString());
		response.sendError(HttpServletResponse.SC_FORBIDDEN);
	}
}
