package cn.blmdz.hunt.engine.mapping;

import java.util.Map;

import cn.blmdz.hunt.engine.config.model.Service;

public class EurekaExecutor extends Executor<Object> {
//	@Autowired
//	private RestTemplate restTemplate;

	@Override
	public boolean detectType(Service service) {
		return false;
	}

	@Override
	public Object exec(Service service, Map<String, Object> params) {
		return null;
	}
}
