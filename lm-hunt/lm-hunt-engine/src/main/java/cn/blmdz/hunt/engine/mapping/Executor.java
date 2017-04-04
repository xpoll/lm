package cn.blmdz.hunt.engine.mapping;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.engine.MessageSources;
import cn.blmdz.hunt.engine.config.model.Service;

public abstract class Executor<T> {
	private static final Logger log = LoggerFactory.getLogger(Executor.class);
	@Autowired(required = false)
	protected MessageSources messageSources;

	public abstract boolean detectType(Service service);

	public abstract T exec(Service service, Map<String, Object> params);

	protected Object unwrapResponse(T result) {
		if (result == null) {
			return null;
		} else if (result instanceof Response) {
			Response<?> resp = (Response<?>) result;
			if (resp.isSuccess()) {
				return resp.getResult();
			} else {
				log.error("failed to execute service, error code:{}", resp.getError());
				if (this.messageSources != null) {
					throw new ServiceException(this.messageSources.get(resp.getError()));
				} else {
					throw new ServiceException(resp.getError());
				}
			}
		} else {
			return result;
		}
	}
}
