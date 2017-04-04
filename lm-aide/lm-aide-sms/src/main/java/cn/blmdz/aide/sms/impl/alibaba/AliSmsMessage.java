package cn.blmdz.aide.sms.impl.alibaba;

import java.io.Serializable;
import java.util.Map;

import lombok.Data;

@Data
public class AliSmsMessage implements Serializable {
	private static final long serialVersionUID = 1L;
	private String smsName;
	private String smsTemplate;
	private Map<?, ?> smsParam;

}
