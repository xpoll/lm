package cn.blmdz.aide.pay.channel.kjtpay.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import lombok.Data;

@Data
public class KjtpayTransInfo implements Serializable {
	private static final long serialVersionUID = 1L;
	private static final BigDecimal ratio = new BigDecimal("100");
	private Long id;
	private String outerNo;
	private String origOuterNo;
	private String innerNo;
	private String type;
	private Date orderAt;
	private Date paidAt;
	private String amount;
	private String rate;
	private String rateFee;
	private String status;
	private Date createdAt;
	private Date updatedAt;

	public Long amountToFen() {
		if (this.amount == null) {
			return Long.valueOf(0L);
		} else {
			BigDecimal money = new BigDecimal(this.amount);
			return Long.valueOf(money.multiply(ratio).longValue());
		}
	}
}
