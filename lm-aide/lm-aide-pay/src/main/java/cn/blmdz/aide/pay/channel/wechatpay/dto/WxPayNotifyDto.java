package cn.blmdz.aide.pay.channel.wechatpay.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.thoughtworks.xstream.annotations.XStreamAlias;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@XStreamAlias("xml")
public class WxPayNotifyDto {
	@XStreamAlias("return_code")
	private String returnCode;
	@XStreamAlias("return_msg")
	private String returnMsg;
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
	@XStreamAlias("result_code")
	private String resultCode;
	@XStreamAlias("err_code")
	private String errCode;
	@XStreamAlias("err_code_des")
	private String errCodeDes;
	@XStreamAlias("openid")
	private String openid;
	@XStreamAlias("is_subscribe")
	private String isSubscribe;
	@XStreamAlias("bank_type")
	private String bankType;
	@XStreamAlias("total_fee")
	private Integer totalFee;
	@XStreamAlias("coupon_fee")
	private Integer couponFee;
	@XStreamAlias("fee_type")
	private String feeType;
	@XStreamAlias("transaction_id")
	private String transactionId;
	@XStreamAlias("out_trade_no")
	private String outTradeNo;
	@XStreamAlias("attach")
	private String attach;
	@XStreamAlias("trade_type")
	private String tradeType;
	@XStreamAlias("prepay_id")
	private String prepayId;
	@XStreamAlias("code_url")
	private String codeUrl;
	@XStreamAlias("time_end")
	private String timeEnd;

	@JsonIgnore
	public boolean isSuccess() {
		return Objects.equal(WxPayNotifyDto.Code.SUCCESS, WxPayNotifyDto.Code.from(this.resultCode))
				&& Objects.equal(WxPayNotifyDto.Code.SUCCESS, WxPayNotifyDto.Code.from(this.returnCode));
	}

	@JsonIgnore
	public String getErrorMsg() {
		return Strings.isNullOrEmpty(this.returnMsg) ? this.errCodeDes : this.returnMsg;
	}

	@Getter
	@AllArgsConstructor
	public static enum Code {
		SUCCESS("SUCCESS"), FAIL("FAIL");

		private String value;

		public static WxPayNotifyDto.Code from(String value) {
			for (WxPayNotifyDto.Code code : values()) {
				if (Objects.equal(value, code.value)) {
					return code;
				}
			}

			return null;
		}
	}

	@Getter
	@AllArgsConstructor
	public static enum ErrorCode {
		NOAUTH("NOAUTH", "商户无权限"), NOTENOUGH("NOTENOUGH", "余额不足"), NOTSUPORTCARD("NOTSUPORTCARD", "不支持卡类型"), ORDERPAID(
				"ORDERPAID", "商户订单已支付"), ORDERCLOSED("ORDERCLOSED",
						"订单已关闭"), BANKERROR("BANKERROR", "银行系统异常"), SYSTEMERROR("SYSTEMERROR", "系统错误错");

		private String value;
		private String display;

		public static WxPayNotifyDto.ErrorCode from(String value) {
			for (WxPayNotifyDto.ErrorCode code : values()) {
				if (Objects.equal(value, code.value)) {
					return code;
				}
			}

			return null;
		}
	}
}
