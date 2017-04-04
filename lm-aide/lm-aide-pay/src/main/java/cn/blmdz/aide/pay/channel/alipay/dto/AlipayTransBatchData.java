package cn.blmdz.aide.pay.channel.alipay.dto;

import lombok.Data;

@Data
public class AlipayTransBatchData {
	private String batchNo;
	private String account;
	private String accountName;
	private Integer fee;
	private String remark;
}
