package cn.blmdz.aide.pay.common;

import java.beans.ConstructorProperties;

import lombok.Data;
import lombok.NonNull;

@Data
public class Refund {
	@NonNull
	protected Long refundTrackId;
	protected String tradeNo;
	protected String paymentCode;
	protected String refundNo;
	protected Integer refundAmount;
	protected Integer totalFee;
	protected Long sellerId;
	@NonNull
	protected String payChannel;

	@ConstructorProperties({ "refundTrackId", "payChannel" })
	public Refund(@NonNull Long refundTrackId, @NonNull String payChannel) {
		if (refundTrackId == null) {
			throw new NullPointerException("refundTrackId");
		} else {
			this.refundTrackId = refundTrackId;
			this.payChannel = payChannel;
		}
	}
}
