package cn.blmdz.aide.pay.channel.kjtpay.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class KjtTransferToken {
	private String pid;
	private String pfxPath;
	private String keyPassword;
	private String gateway;
	private String cerPath;
}
