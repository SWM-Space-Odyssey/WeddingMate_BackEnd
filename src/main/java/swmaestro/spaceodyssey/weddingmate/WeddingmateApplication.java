package swmaestro.spaceodyssey.weddingmate;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.servers.Server;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.scheduling.annotation.EnableScheduling;

@OpenAPIDefinition(servers = {@Server(url = "/")})
@EnableScheduling
@EnableJpaAuditing
@SpringBootApplication
public class WeddingmateApplication {

	public static void main(String[] args) {
		SpringApplication.run(WeddingmateApplication.class, args);
	}

}
