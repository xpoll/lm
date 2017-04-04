package cn.blmdz.aide.pay.common;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

@Data
public class Payment implements Serializable {
	private static final long serialVersionUID = 1L;
	private String tradeNo;
	private String channel;
	private String bank;
	private Integer fee;
	private String subject;
	private String content;
	private String host;
	private String sellerId;
	private Long buyerId;
	private Date expiredAt;
	private Long expiredPeriod;
	private String systemNo;
}
