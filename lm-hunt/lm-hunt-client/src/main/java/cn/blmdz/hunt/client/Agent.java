package cn.blmdz.hunt.client;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

public abstract interface Agent {
	public abstract LinkedHashMap<String, ParamInfo> getParamsInfo(String key);

	public abstract WrapResp call(String key, Map<String, Object> params, Map<String, Object> context);

	@NoArgsConstructor
	@AllArgsConstructor
	public static class ParamInfo implements Serializable {
		private static final long serialVersionUID = -3755000483102791609L;
		@Getter
		private boolean isOptional;
		@Getter
		private String className;
	}
}