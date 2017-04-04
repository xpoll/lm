package cn.blmdz.aide.pay.common;

import lombok.Data;

@Data
public class BaseToken {
	private String gateway;
	private String notifyUrl;
	private String returnUrl;
	protected String refundNotifyUrl;
}
