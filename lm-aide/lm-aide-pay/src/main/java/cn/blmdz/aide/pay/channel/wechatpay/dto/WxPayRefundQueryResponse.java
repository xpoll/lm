package cn.blmdz.aide.pay.channel.wechatpay.dto;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.thoughtworks.xstream.annotations.XStreamAlias;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.List;

@Getter
@Setter
@ToString
@XStreamAlias("xml")
public class WxPayRefundQueryResponse {
	@XStreamAlias("return_code")
	private String returnCode;
	@XStreamAlias("return_msg")
	private String returnMsg;
	@XStreamAlias("result_code")
	private String resultCode;
	@XStreamAlias("err_code")
	private String errCode;
	@XStreamAlias("err_code_des")
	private String errCodeDes;
	@XStreamAlias("appid")
	private String appid;
	@XStreamAlias("mch_id")
	private String mchId;
	@XStreamAlias("sub_mch_id")
	private String subMchId;
	@XStreamAlias("device_info")
	private String deviceInfo;
	@XStreamAlias("nonce_str")
	private String nonceStr;
	@XStreamAlias("sign")
	private String sign;
	@XStreamAlias("transaction_id")
	private String transactionId;
	@XStreamAlias("out_trade_no")
	private String outTradeNo;
	@XStreamAlias("refund_count")
	private String refundCount;
	private List<WxPayRefundDto> wxPayRefundDtos;

	public boolean isSuccess() {
		return Objects.equal(WxPayRefundQueryResponse.Code.SUCCESS, WxPayRefundQueryResponse.Code.from(this.resultCode))
				&& Objects.equal(WxPayRefundQueryResponse.Code.SUCCESS,
						WxPayRefundQueryResponse.Code.from(this.returnCode));
	}

	public String getErrorMsg() {
		return Strings.isNullOrEmpty(this.returnMsg) ? this.errCodeDes : this.returnMsg;
	}

	@Getter
	@AllArgsConstructor
	public static enum Code {
		SUCCESS("SUCCESS"), FAIL("FAIL");

		private String value;

		public static WxPayRefundQueryResponse.Code from(String value) {
			for (WxPayRefundQueryResponse.Code code : values()) {
				if (Objects.equal(value, code.value)) {
					return code;
				}
			}

			return null;
		}
	}
}
