package cn.blmdz.aide.pay.channel.wechatpay.dto;

import com.google.common.base.Strings;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Date;
import java.util.List;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

@Getter
@Setter
@ToString
public class WxPayBillDto {
	private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
	private Date tradeTime;
	private String appid;
	private String mchId;
	private String subMchId;
	private String deviceInfo;
	private String transactionId;
	private String outTradeNo;
	private String openId;
	private String tradeType;
	private String tradeStatus;
	private String bankType;
	private String feeType;
	private String totalFee;
	private String couponFee;
	private Date refundApplyDate;
	private Date refundSuccessDate;
	private String refundId;
	private String outRefundNo;
	private String refundFee;
	private String couponRefundFee;
	private String refundChannel;
	private String refundStatus;
	private String body;
	private String attach;
	private String poundageFee;
	private String rate;
	private String bankOrderNo;
	private String tradeInfo;

	public static WxPayBillDto all(List<String> cols) {
		WxPayBillDto dto = new WxPayBillDto();
		if (!Strings.isNullOrEmpty(cols.get(0))) {
			dto.setTradeTime(DFT.parseDateTime(cols.get(0)).toDate());
		}

		dto.setAppid(cols.get(1));
		dto.setMchId(cols.get(2));
		dto.setSubMchId(cols.get(3));
		dto.setDeviceInfo(cols.get(4));
		dto.setTransactionId(cols.get(5));
		dto.setOutTradeNo(cols.get(6));
		dto.setOpenId(cols.get(7));
		dto.setTradeType(cols.get(8));
		dto.setTradeStatus(cols.get(9));
		dto.setBankType(cols.get(10));
		dto.setFeeType(cols.get(11));
		dto.setTotalFee(cols.get(12));
		dto.setCouponFee(cols.get(13));
		dto.setRefundId(cols.get(14));
		dto.setOutRefundNo(cols.get(15));
		dto.setRefundFee(cols.get(16));
		dto.setCouponRefundFee(cols.get(17));
		dto.setRefundChannel(cols.get(18));
		dto.setRefundStatus(cols.get(19));
		dto.setBody(cols.get(20));
		dto.setAttach(cols.get(21));
		dto.setPoundageFee(cols.get(22));
		dto.setRate(cols.get(23));
		return dto;
	}

	public static WxPayBillDto success(List<String> cols) {
		WxPayBillDto dto = new WxPayBillDto();
		if (!Strings.isNullOrEmpty(cols.get(0))) {
			dto.setTradeTime(DFT.parseDateTime(cols.get(0)).toDate());
		}

		dto.setAppid(cols.get(1));
		dto.setMchId(cols.get(2));
		dto.setSubMchId(cols.get(3));
		dto.setDeviceInfo(cols.get(4));
		dto.setTransactionId(cols.get(5));
		dto.setOutTradeNo(cols.get(6));
		dto.setOpenId(cols.get(7));
		dto.setTradeType(cols.get(8));
		dto.setTradeStatus(cols.get(9));
		dto.setBankType(cols.get(10));
		dto.setFeeType(cols.get(11));
		dto.setTotalFee(cols.get(12));
		dto.setCouponFee(cols.get(13));
		dto.setBody(cols.get(14));
		dto.setAttach(cols.get(15));
		dto.setPoundageFee(cols.get(16));
		dto.setRate(cols.get(17));
		return dto;
	}

	public static WxPayBillDto refund(List<String> cols) {
		WxPayBillDto dto = new WxPayBillDto();
		if (!Strings.isNullOrEmpty(cols.get(0))) {
			dto.setTradeTime(DFT.parseDateTime(cols.get(0)).toDate());
		}

		dto.setAppid(cols.get(1));
		dto.setMchId(cols.get(2));
		dto.setSubMchId(cols.get(3));
		dto.setDeviceInfo(cols.get(4));
		dto.setTransactionId(cols.get(5));
		dto.setOutTradeNo(cols.get(6));
		dto.setOpenId(cols.get(7));
		dto.setTradeType(cols.get(8));
		dto.setTradeStatus(cols.get(9));
		dto.setBankType(cols.get(10));
		dto.setFeeType(cols.get(11));
		dto.setTotalFee(cols.get(12));
		dto.setCouponFee(cols.get(13));
		if (!Strings.isNullOrEmpty(cols.get(14))) {
			dto.setRefundApplyDate(DFT.parseDateTime(cols.get(14)).toDate());
		}

		if (!Strings.isNullOrEmpty(cols.get(15))) {
			dto.setRefundSuccessDate(DFT.parseDateTime(cols.get(15)).toDate());
		}

		dto.setRefundId(cols.get(16));
		dto.setOutRefundNo(cols.get(17));
		dto.setRefundFee(cols.get(18));
		dto.setCouponRefundFee(cols.get(19));
		dto.setRefundChannel(cols.get(20));
		dto.setRefundStatus(cols.get(21));
		dto.setBody(cols.get(22));
		dto.setAttach(cols.get(23));
		dto.setPoundageFee(cols.get(24));
		dto.setRate(cols.get(25));
		return dto;
	}
}
