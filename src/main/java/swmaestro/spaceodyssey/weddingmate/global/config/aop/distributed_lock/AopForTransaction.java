package swmaestro.spaceodyssey.weddingmate.global.config.aop.distributed_lock;

import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Component
public class AopForTransaction {

	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public Object proceed(final ProceedingJoinPoint joinPoint) throws Throwable {
		log.info("aop = {}", joinPoint.getSignature());
		return joinPoint.proceed();
	}
}