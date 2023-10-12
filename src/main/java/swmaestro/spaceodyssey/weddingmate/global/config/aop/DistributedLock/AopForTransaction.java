package swmaestro.spaceodyssey.weddingmate.global.config.aop.DistributedLock;

import org.aspectj.lang.ProceedingJoinPoint;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
public class AopForTransaction {

	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public Object proceed(final ProceedingJoinPoint joinPoint) throws Throwable {
		System.out.println("aop = {}"+joinPoint.getSignature());
		return joinPoint.proceed();
	}
}
