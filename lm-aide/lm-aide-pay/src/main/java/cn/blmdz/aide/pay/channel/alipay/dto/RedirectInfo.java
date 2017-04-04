package cn.blmdz.aide.pay.channel.alipay.dto;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RedirectInfo implements Serializable {
	private static final long serialVersionUID = 1L;
	private boolean success;
	private String error;
	private Boolean isRedirectNow;
	private String channel;
	private String result;

	public void setResult(String result) {
		this.success = true;
		this.result = result;
	}

	public void setError(String error) {
		this.success = false;
		this.error = error;
	}
}
