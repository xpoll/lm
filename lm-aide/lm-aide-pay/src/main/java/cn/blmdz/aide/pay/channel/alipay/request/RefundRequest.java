package cn.blmdz.aide.pay.channel.alipay.request;

import java.net.URLEncoder;
import java.util.Date;
import java.util.List;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

import cn.blmdz.aide.pay.channel.alipay.dto.AlipayRefundData;
import cn.blmdz.aide.pay.utils.GenerateRandom;
import cn.blmdz.home.common.util.Arguments;

public class RefundRequest extends Request {
	private static final Logger log = LoggerFactory.getLogger(RefundRequest.class);
	private static final Joiner DETAIL_JOINER = Joiner.on("^").skipNulls();
	private static final Joiner REFUND_JOINER = Joiner.on("#").skipNulls();
	private static final DateTimeFormatter DFT_BATCH = DateTimeFormat.forPattern("yyyyMMddHHmmss");
	private static final DateTimeFormatter DFT_REFUND = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");

	private RefundRequest(AlipayToken alipayToken, String service) {
		super(alipayToken);
		this.params.put("service", service);
		this.params.put("refund_date", DFT_REFUND.print(DateTime.now()));
	}

	public static RefundRequest buildWithNoPwd(AlipayToken alipayToken) {
		return new RefundRequest(alipayToken, "refund_fastpay_by_platform_nopwd");
	}

	public static RefundRequest buildWithPwd(AlipayToken alipayToken) {
		return new RefundRequest(alipayToken, "refund_fastpay_by_platform_pwd");
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

	public RefundRequest notify(CallBack notify) {
		if (Arguments.notNull(notify)) {
			this.params.put("notify_url", notify);
		}

		return this;
	}

	public RefundRequest batch(String batchNo) {
		Preconditions.checkArgument(!Strings.isNullOrEmpty(batchNo), "alipay.refund.batch.no.empty");
		this.params.put("batch_no", batchNo);
		return this;
	}

	public RefundRequest detail(List<AlipayRefundData> refunds) {
		String detail = this.getRefundDetail(refunds);
		this.params.put("detail_data", detail);
		this.params.put("batch_num", refunds.size() + "");
		return this;
	}

	public Boolean refund() {
		String url = super.url();
		log.debug("refund url: {}", url);
		String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
		log.debug("refund result: {}", body);
		return this.convertToResponse(body);
	}

	public static String toBatchNo(Date refundAt, Long refundTrackId) {
		Preconditions.checkNotNull(refundTrackId, "order.item.id.null");
		Preconditions.checkNotNull(Boolean.valueOf(refundAt != null), "refund.at.null");
		String prefix = DFT_BATCH.print(new DateTime(refundAt));
		String suffix = refundTrackId.toString();
		Integer trackLenght = Integer.valueOf(suffix.length());
		String random = GenerateRandom.rand(18 - trackLenght.intValue());
		return prefix + random + suffix;
	}

	public void sign() {
		try {
			super.sign();
			String refundDate = (String) this.params.get("refund_date");
			if (!Strings.isNullOrEmpty(refundDate)) {
				this.params.put("refund_date", URLEncoder.encode(refundDate, "utf-8"));
			}

			String detailData = (String) this.params.get("detail_data");
			if (!Strings.isNullOrEmpty(detailData)) {
				this.params.put("detail_data", URLEncoder.encode(detailData, "utf-8"));
			}

		} catch (Exception var3) {
			throw new RuntimeException(var3);
		}
	}
}
