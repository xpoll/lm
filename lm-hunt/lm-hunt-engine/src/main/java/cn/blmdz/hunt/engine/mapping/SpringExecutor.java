package cn.blmdz.hunt.engine.mapping;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.convert.support.DefaultConversionService;
import org.springframework.stereotype.Component;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.hunt.client.ParamUtil;
import cn.blmdz.hunt.client.ParamUtil.MethodInfo;
import cn.blmdz.hunt.client.ParamUtil.ParamInfo;
import cn.blmdz.hunt.engine.config.model.Service;
import cn.blmdz.hunt.protocol.Export;

@Component
public class SpringExecutor extends Executor {
	private static final Logger log = LoggerFactory.getLogger(SpringExecutor.class);
	private final LoadingCache<String, MethodInfo> methodInfos;
//	private DefaultConversionService converter = new DefaultConversionService();
	@Autowired
	private ApplicationContext applicationContext;

	public SpringExecutor() {
		CacheLoader<String, MethodInfo> loader = new CacheLoader<String, MethodInfo>() {
			public MethodInfo load(String key) throws Exception {
				List<String> parts = Splitters.COLON.splitToList(key);
				if (parts.size() != 2) {
					throw new IllegalArgumentException(
							"bad api format,should be interfaceName:methodName,but is: " + key);
				} else {
					Class<?> klass = Class.forName((String) parts.get(0));
					Object bean = SpringExecutor.this.applicationContext.getBean(klass);
					Method method = SpringExecutor.this.findMethodByName(klass, (String) parts.get(1));
					if (method == null) {
						throw new NoSuchMethodException("failed to find method: " + key);
					} else {
						return ParamUtil.getMethodInfo(bean, method);
					}
				}
			}
		};
		this.methodInfos = CacheBuilder.newBuilder().build(loader);
	}

	public boolean detectType(Service service) {
		try {
			MethodInfo result = (MethodInfo) this.methodInfos.getUnchecked(service.getUri());
			return result != null;
		} catch (Exception var3) {
			log.warn(
					"detect spring service type for [{}] failed, it\'s maybe not an exception that need to attention. {}",
					service.getUri(), var3.getMessage());
			log.debug("detect spring service type for [{}] failed, debug info: {}", service.getUri(),
					Throwables.getStackTraceAsString(var3));
			return false;
		}
	}

	public Object exec(Service service, Map params) {
		String api = service.getUri();
		if (Strings.isNullOrEmpty(api)) {
			return null;
		} else {
			MethodInfo methodInfo = (MethodInfo) this.methodInfos.getUnchecked(api);
			LinkedHashMap<String, ParamInfo> paramsInfo = methodInfo.getParams();
			Object[] concernedParams = new Object[paramsInfo.size()];
			int index = 0;

			for (String paramName : paramsInfo.keySet()) {
				ParamInfo paramInfo = (ParamInfo) paramsInfo.get(paramName);
				Object param = ParamConverter.convertParam(paramName, paramInfo.getClazz(), params,
						paramInfo.isOptional());
				concernedParams[index++] = ParamUtil.convert(param, paramInfo, params);
			}

			Object object;
			try {
				object = methodInfo.getMethod().invoke(methodInfo.getBean(), concernedParams);
			} catch (IllegalAccessException var12) {
				log.error("illegal access method, service: {}", service, var12);
				throw new ServiceException(var12);
			} catch (InvocationTargetException var13) {
				log.error("invocation target exception, service: {}", service, var13);
				if (var13.getTargetException() instanceof RuntimeException) {
					throw (RuntimeException) var13.getTargetException();
				}

				throw new ServiceException(var13.getTargetException().getMessage(), var13);
			}

			return this.unwrapResponse(object);
		}
	}

	private Method findMethodByName(Class beanClazz, String methodName) {
		Method[] methods = beanClazz.getMethods();

		for (Method method : methods) {
			if (method.getName().equals(methodName) && method.getAnnotation(Export.class) != null) {
				return method;
			}
		}

		return null;
	}

	protected static class ServiceInfo {
		private final Class klass;
		private final Method method;
		private final Class[] types;
		private final String[] paramNames;

		public ServiceInfo(Class klass, Method method, Class[] types, String[] paramNames) {
			this.klass = klass;
			this.method = method;
			this.types = types;
			this.paramNames = paramNames;
		}

		public Class getKlass() {
			return this.klass;
		}

		public Method getMethod() {
			return this.method;
		}

		public Class[] getTypes() {
			return this.types;
		}

		public String[] getParamNames() {
			return this.paramNames;
		}
	}
}
