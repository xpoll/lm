package cn.blmdz.aide.pay.channel.kjtpay.request;

import java.util.Date;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.base.Splitter.MapSplitter;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

import cn.blmdz.aide.pay.channel.alipay.dto.AlipayRefundData;
import cn.blmdz.home.common.util.Arguments;

public class KjtRefundRequest extends Request {
	private static final Logger log = LoggerFactory.getLogger(KjtRefundRequest.class);
	private static final Joiner DETAIL_JOINER = Joiner.on("^").skipNulls();
	private static final Joiner REFUND_JOINER = Joiner.on("#").skipNulls();
	private static final DateTimeFormatter DFT_BATCH = DateTimeFormat.forPattern("yyyyMMdd");

	private KjtRefundRequest(KjtToken token) {
		super(token);
		this.params.put("service", "create_refund");
		this.params.put("operator_id", "");
		this.params.put("royalty_parameters", "");
		this.params.put("refund_ensure_amount", "");
		this.params.put("deposit_amount", "");
	}

	public static KjtRefundRequest build(KjtToken kjtToken) {
		return new KjtRefundRequest(kjtToken);
	}

	private String getRefundDetail(List<AlipayRefundData> refunds) {
		List<String> refundDetails = Lists.newArrayListWithCapacity(refunds.size());

		for (AlipayRefundData refund : refunds) {
			String tradeNo = refund.getTradeNo();
			Integer refundAmount = refund.getRefundAmount();
			String reason = refund.getReason();
			String detail = DETAIL_JOINER.join(tradeNo,
					DECIMAL_FORMAT.format((double) refundAmount.intValue() / 100.0D), new Object[] { reason });
			refundDetails.add(detail);
		}

		return REFUND_JOINER.join(refundDetails);
	}

	public KjtRefundRequest notify(String notify) {
		if (Arguments.notNull(notify)) {
			this.params.put("notify_url", notify);
		}

		return this;
	}

	public KjtRefundRequest returnUrl(String forward) {
		if (Arguments.notNull(forward)) {
			this.params.put("return_url", forward);
		}

		return this;
	}

	public KjtRefundRequest setOuter(String outer) {
		Preconditions.checkArgument(!Strings.isNullOrEmpty(outer), "outer.trade.no.empty");
		this.params.put("outer_trade_no", outer);
		return this;
	}

	public KjtRefundRequest setOrigOuter(String orig_outer) {
		Preconditions.checkArgument(!Strings.isNullOrEmpty(orig_outer), "orig.outer.trade.no.empty");
		this.params.put("orig_outer_trade_no", orig_outer);
		return this;
	}

	public KjtRefundRequest setAmount(String amount) {
		Preconditions.checkArgument(!Arguments.isNull(amount), "refund.amount.no.null");
		this.params.put("refund_amount", amount);
		return this;
	}

	public KjtRefundRequest setSign(String sign) {
		Preconditions.checkArgument(Arguments.notNull(sign), "kjt.pay.sign.empty");
		this.params.put("sign", sign);
		return this;
	}

	public KjtRefundRequest setSignType(String type) {
		Preconditions.checkArgument(Arguments.notNull(type), "kjt.pay.sign_type.empty");
		this.params.put("sign_type", type);
		return this;
	}

	public KjtRefundRequest detail(List<AlipayRefundData> refunds) {
		String detail = this.getRefundDetail(refunds);
		this.params.put("detail_data", detail);
		this.params.put("batch_num", refunds.size() + "");
		return this;
	}

	public Boolean refund() {
		String url = super.url();
		log.debug("kjtpay refund url: {}", url);
		String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
		log.debug("kjtpay refund result: {}", body);
		MapSplitter splitter = Splitter.on("&").withKeyValueSeparator("=");
		Map<String, String> map = splitter.split(body);
		return Boolean.valueOf(((String) map.get("is_success")).equals("T"));
	}

	public static String toBatchNo(Date refundAt, Long orderItemId) {
		Preconditions.checkNotNull(orderItemId, "order.item.id.null");
		Preconditions.checkNotNull(Boolean.valueOf(refundAt != null), "refund.at.null");
		String prefix = DFT_BATCH.print(new DateTime(refundAt));
		String suffix = Strings.padStart(orderItemId.toString(), 24, '0');
		return prefix + suffix;
	}

	public static Long fromBatchNo(String batchNo) {
		Preconditions.checkArgument(!Strings.isNullOrEmpty(batchNo), "batch.no.empty");
		Preconditions.checkArgument(batchNo.length() == 32, "batch.no.length.illegal");
		int len = batchNo.length();
		return Long.valueOf(batchNo.substring(len - 24, len));
	}
}
