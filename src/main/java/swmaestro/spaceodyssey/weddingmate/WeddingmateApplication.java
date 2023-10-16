package swmaestro.spaceodyssey.weddingmate;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.servers.Server;

@OpenAPIDefinition(servers = {@Server(url = "/")})
@EnableJpaAuditing
@SpringBootApplication
public class WeddingmateApplication {

	public static void main(String[] args) {
		SpringApplication.run(WeddingmateApplication.class, args);
	}

}
