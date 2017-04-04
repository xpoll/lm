package cn.blmdz.aide.sms.impl.emay;

import java.io.Serializable;

import lombok.Data;

@Data
public class EmayToken implements Serializable {
	private static final long serialVersionUID = 1L;
	private String account;
	private String password;
	private String sendUrl;
	private String queryUrl;
	private String registerUrl;
}
