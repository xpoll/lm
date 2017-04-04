package cn.blmdz.aide.pay.channel.kjtpay.request;

import java.util.Date;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.MoreObjects;

public class KjtPageQueryRequest extends Request {
	private static final Logger log = LoggerFactory.getLogger(KjtPageQueryRequest.class);
	private static final DateTimeFormatter DFT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
	private static final DateTimeFormatter DFTSETTLE = DateTimeFormat.forPattern("yyyyMMdd");
	@SuppressWarnings("unused")
	private Date start;
	@SuppressWarnings("unused")
	private Date end;

	private KjtPageQueryRequest(KjtToken token) {
		super(token);
		this.params.put("service", "query_settled_data");
	}

	public static KjtPageQueryRequest build(KjtToken token) {
		return new KjtPageQueryRequest(token);
	}

	public KjtPageQueryRequest settleDate(Date settleDate) {
		this.params.put("settle_date", DFTSETTLE.print(new DateTime(settleDate)));
		return this;
	}

	public KjtPageQueryRequest start(Date start) {
		this.params.put("gmt_start_time", DFT.print(new DateTime(start)));
		this.start = start;
		return this;
	}

	public KjtPageQueryRequest end(Date end) {
		this.params.put("gmt_end_time", DFT.print(new DateTime(end)));
		this.end = end;
		return this;
	}

	public KjtPageQueryRequest pageNo(String pageNo) {
		pageNo = (String) MoreObjects.firstNonNull(pageNo, Integer.valueOf(1));
		this.params.put("page_no", pageNo);
		return this;
	}

	public KjtPageQueryRequest pageSize(String pageSize) {
		pageSize = (String) MoreObjects.firstNonNull(pageSize, Integer.valueOf(1000));
		this.params.put("page_size", pageSize);
		return this;
	}

	public KjtPageQueryRequest tradeNo(String tradeNo) {
		this.params.put("trade_no", tradeNo);
		return this;
	}

	public KjtPageQueryRequest merchantOutOrderNo(String merchantOutOrderNo) {
		this.params.put("merchant_out_order_no", merchantOutOrderNo);
		return this;
	}

	public String query() {
		String url = super.url();
		log.debug("query url: {}", url);
		String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
		return body;
	}
}
