package cn.blmdz.aide.pay.channel.kjtpay.pojo;

import java.util.Map;

import com.google.common.collect.Maps;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class VerifyResult {
	public static final String exMsg = "exMsg";
	public static final String msg = "msg";
	public static final String identityNo = "identityNo";
	private boolean success = false;
	private boolean needPostCheck = false;
	private Map<String, Object> info = Maps.newHashMap();

	public VerifyResult(boolean success) {
		this.success = success;
	}

	public void addInfo(String key, Object val) {
		this.info.put(key, val);
	}

	public String toString() {
		return "VerifyResult [success=" + this.success + ", info=" + this.info + "]";
	}
}
