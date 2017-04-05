package cn.blmdz.aide.pay.channel.wechatpay.request;

import cn.blmdz.aide.pay.common.BaseToken;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper=false)
public class WxToken extends BaseToken {
	private String appId;
	private String secret;
	private String mchId;
	private String paternerkey;
	private String gateway;
	private String refundGateway;
	private String dowloadBillUrl;
	private String queryRefundGateway;
}
