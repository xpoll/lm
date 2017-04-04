package cn.blmdz.aide.pay.channel.alipay.request;

import java.net.URLEncoder;
import java.util.Date;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.MoreObjects;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.thoughtworks.xstream.XStream;

import cn.blmdz.aide.pay.channel.alipay.dto.AlipaySettlementResponse;
import cn.blmdz.home.common.util.Arguments;

public class PageQueryRequest extends Request {
	private static final Logger log = LoggerFactory.getLogger(PageQueryRequest.class);
	private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
	private static final XStream xstream = new XStream();
	private Date start;
	private Date end;

	private PageQueryRequest(AlipayToken alipayToken) {
		super(alipayToken);
		this.params.put("service", "account.page.query");
	}

	public static PageQueryRequest build(AlipayToken alipayToken) {
		return new PageQueryRequest(alipayToken);
	}

	public PageQueryRequest start(Date start) {
		this.params.put("gmt_start_time", DFT.print(new DateTime(start)));
		this.start = start;
		return this;
	}

	public PageQueryRequest end(Date end) {
		this.params.put("gmt_end_time", DFT.print(new DateTime(end)));
		this.end = end;
		return this;
	}

	public PageQueryRequest pageNo(Integer pageNo) {
		pageNo = (Integer) MoreObjects.firstNonNull(pageNo, Integer.valueOf(1));
		this.params.put("page_no", pageNo);
		return this;
	}

	public PageQueryRequest pageSize(Integer pageSize) {
		pageSize = (Integer) MoreObjects.firstNonNull(pageSize, Integer.valueOf(1000));
		this.params.put("page_size", pageSize);
		return this;
	}

	public PageQueryRequest tradeNo(String tradeNo) {
		this.params.put("trade_no", tradeNo);
		return this;
	}

	public PageQueryRequest merchantOutOrderNo(String merchantOutOrderNo) {
		this.params.put("merchant_out_order_no", merchantOutOrderNo);
		return this;
	}

	public String queryUrl() {
		if (!this.params.containsKey("trade_no") && !this.params.containsKey("merchant_out_order_no")) {
			Preconditions.checkArgument(Arguments.notNull(this.start), "start.can.not.be.empty");
			Preconditions.checkArgument(Arguments.notNull(this.end), "end.can.not.be.empty");
			DateTime startAt = new DateTime(this.start);
			DateTime endAt = new DateTime(this.end);
			Preconditions
					.checkArgument(endAt.minusHours(24).isBefore(startAt) || endAt.minusHours(24).isEqual(startAt));
		}

		String url = super.url();
		log.debug("query url: {}", url);
		return url;
	}

	public static AlipaySettlementResponse queryResultToObject(String result) {
		log.debug("alipay trans is: {}", result);
		return transform(result);
	}

	public AlipaySettlementResponse query() {
		if (!this.params.containsKey("trade_no") && !this.params.containsKey("merchant_out_order_no")) {
			Preconditions.checkArgument(Arguments.notNull(this.start), "start.can.not.be.empty");
			Preconditions.checkArgument(Arguments.notNull(this.end), "end.can.not.be.empty");
			DateTime startAt = new DateTime(this.start);
			DateTime endAt = new DateTime(this.end);
			Preconditions
					.checkArgument(endAt.minusHours(24).isBefore(startAt) || endAt.minusHours(24).isEqual(startAt));
		}

		String url = super.url();
		log.debug("query url: {}", url);
		String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
		log.debug("return body content: {}", body);
		return transform(body);
	}

	private static AlipaySettlementResponse transform(String xml) {
		return (AlipaySettlementResponse) xstream.fromXML(xml);
	}

	public void sign() {
		try {
			super.sign();
			String begin = (String) this.params.get("gmt_start_time");
			if (!Strings.isNullOrEmpty(begin)) {
				this.params.put("gmt_start_time", URLEncoder.encode(begin, "utf-8"));
			}

			String end = (String) this.params.get("gmt_end_time");
			if (!Strings.isNullOrEmpty(end)) {
				this.params.put("gmt_end_time", URLEncoder.encode(end, "utf-8"));
			}

		} catch (Exception var3) {
			throw new RuntimeException(var3);
		}
	}

	static {
		xstream.autodetectAnnotations(true);
		xstream.processAnnotations(AlipaySettlementResponse.class);
	}
}
