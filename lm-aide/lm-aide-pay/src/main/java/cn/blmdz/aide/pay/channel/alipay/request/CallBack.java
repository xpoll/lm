package cn.blmdz.aide.pay.channel.alipay.request;

import java.util.Map;

import com.google.common.base.Preconditions;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.JsonMapper;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CallBack {
	private String url;
	private Map<String, Object> params = Maps.newTreeMap();

	public CallBack(String suffix) {
		this.url = suffix;
	}

	public void append(String key, String value) {
		Preconditions.checkArgument(Arguments.notEmpty(key));
		this.params.put(key, value);
	}

	public String toString() {
		Preconditions.checkArgument(Arguments.notNull(this.url));
		return this.params.size() == 0 ? this.url
				: this.url + "?" + "detail=" + JsonMapper.nonDefaultMapper().toJson(this.params);
	}
}
