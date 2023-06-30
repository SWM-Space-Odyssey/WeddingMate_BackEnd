package swmaestro.spaceodyssey.weddingmate;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@EnableJpaAuditing
@SpringBootApplication
public class WeddingmateApplication {

	public static void main(String[] args) {
		SpringApplication.run(WeddingmateApplication.class, args);
	}

}
