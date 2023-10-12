package swmaestro.spaceodyssey.weddingmate.global.config.aop.ExecutionTime;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.StopWatch;

@Component
@Aspect
public class TimeTraceAop {

	Logger logger = LoggerFactory.getLogger(TimeTraceAop.class);

	@Around("@annotation(swmaestro.spaceodyssey.weddingmate.global.config.aop.ExecutionTime.LogExecutionTime)")
	public Object logExecutionTime(ProceedingJoinPoint joinPoint) throws Throwable {
		StopWatch stopWatch = new StopWatch();
		stopWatch.start();
		try {
			return joinPoint.proceed();
		} finally {
			stopWatch.stop();
			logger.info(stopWatch.prettyPrint());
		}
	}
}
