package cn.blmdz.aide.pay.utils;

import com.google.common.base.Strings;
import java.math.BigDecimal;

public class NumberSwitch {
	private static final BigDecimal ratio = new BigDecimal("100");

	public static Long integerToLong(Integer source) {
		return Long.valueOf((new BigDecimal(source.intValue())).longValue());
	}

	public static Integer longToInteger(Long source) {
		return Integer.valueOf((new BigDecimal(source.longValue())).intValue());
	}

	public static Long getCommission(Integer fee, Integer rate) {
		return Long.valueOf((new BigDecimal(fee.intValue()))
				.multiply((new BigDecimal(rate.intValue())).divide(new BigDecimal(10000))).setScale(0, 0).longValue());
	}

	public static Long getAgentCommission(Long fee, Integer rate) {
		return Long.valueOf((new BigDecimal(fee.longValue()))
				.multiply((new BigDecimal(rate.intValue())).divide(new BigDecimal(10000))).setScale(0, 0).longValue());
	}

	public static Integer getTransFormRate(String rate) {
		BigDecimal bd = new BigDecimal(rate);
		Integer alipayRate = Integer.valueOf(bd.multiply(new BigDecimal(10000)).intValue());
		return alipayRate;
	}

	public static Long stringToFen(String price) {
		if (Strings.isNullOrEmpty(price)) {
			return Long.valueOf(0L);
		} else {
			BigDecimal money = new BigDecimal(price);
			return Long.valueOf(money.multiply(ratio).longValue());
		}
	}

	public static String percentToString(String price) {
		BigDecimal money = new BigDecimal(price);
		return money.divide(new BigDecimal(100)).toString();
	}

	public static Long roundUp(String price) {
		if (Strings.isNullOrEmpty(price)) {
			return Long.valueOf(0L);
		} else {
			BigDecimal money = new BigDecimal(price);
			return Long.valueOf(money.setScale(2, 0).multiply(ratio).longValue());
		}
	}

	public static Boolean checkThridFee(Integer totalFee, Long thridFee, String condition) {
		BigDecimal total = (new BigDecimal(totalFee.intValue())).multiply(new BigDecimal(condition));
		BigDecimal thrid = new BigDecimal(thridFee.longValue());
		return Boolean.valueOf(thrid.compareTo(total) != 1);
	}

	public static String getAlipayRate(String outcome, String total) {
		BigDecimal outcomeBg = new BigDecimal(outcome);
		BigDecimal totalBg = new BigDecimal(total);
		BigDecimal bd = outcomeBg.divide(totalBg, 2, 0);
		return bd.toString();
	}

	public static Long reverse(Long source) {
		return Long.valueOf(0L - source.longValue());
	}
}
