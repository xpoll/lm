package cn.blmdz.aide.pay.channel.alipay.request;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.joda.time.DateTime;
import org.joda.time.Minutes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.CharMatcher;
import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.google.common.hash.Hashing;
import com.thoughtworks.xstream.XStream;

import cn.blmdz.aide.pay.driver.CDATAXppDriver;
import cn.blmdz.aide.pay.driver.PojoMapConverter;
import cn.blmdz.home.common.util.Arguments;

public class MobileTokenRequest extends Request {
	private static final Logger log = LoggerFactory.getLogger(MobileTokenRequest.class);
	private static Pattern pattern = Pattern.compile("([&])(.+?)=([^&]*)");
	private static XStream mapxStream = new XStream(new CDATAXppDriver());
	private static XStream errorxStream = new XStream(new CDATAXppDriver());
	private static XStream dataxStream = new XStream(new CDATAXppDriver());
	protected Map<String, Object> tokenParams = Maps.newTreeMap();

	private MobileTokenRequest(AlipayToken alipayToken) {
		super(alipayToken);
		this.params.remove("seller_email");
		this.params.remove("_input_charset");
		this.params.put("service", "alipay.wap.trade.create.direct");
		this.params.put("format", "xml");
		this.params.put("v", "2.0");
		this.tokenParams.put("seller_account_name", alipayToken.getAccount());
	}

	public static MobileTokenRequest build(AlipayToken alipayToken) {
		return new MobileTokenRequest(alipayToken);
	}

	public MobileTokenRequest reqId(String reqId) {
		if (Arguments.notNull(reqId)) {
			this.params.put("req_id", reqId);
		}

		return this;
	}

	public MobileTokenRequest subject(String subject) {
		if (Arguments.notNull(subject)) {
			this.tokenParams.put("subject", subject);
		}

		return this;
	}

	public MobileTokenRequest outTradeNo(String outTradeNo) {
		if (Arguments.notNull(outTradeNo)) {
			this.tokenParams.put("out_trade_no", outTradeNo);
		}

		return this;
	}

	public MobileTokenRequest total(Integer total) {
		Preconditions.checkArgument(Arguments.notNull(total), "alipay.pay.total.empty");
		String fee = DECIMAL_FORMAT.format((double) total.intValue() / 100.0D);
		this.tokenParams.put("total_fee", fee);
		return this;
	}

	public MobileTokenRequest expireTime(Date expireTime) {
		if (expireTime != null) {
			DateTime date = new DateTime(expireTime);
			DateTime now = DateTime.now();
			Integer mins = Integer.valueOf(Minutes.minutesBetween(now, date).getMinutes());
			this.tokenParams.put("pay_expire", mins.toString());
		}

		return this;
	}

	public MobileTokenRequest forward(CallBack forward) {
		if (Arguments.notNull(forward)) {
			this.tokenParams.put("call_back_url", forward.getUrl());
		}

		return this;
	}

	public MobileTokenRequest notify(CallBack notify) {
		if (Arguments.notNull(notify)) {
			this.tokenParams.put("notify_url", notify.getUrl());
		}

		return this;
	}

	private void buildReqData() {
		String xml = mapxStream.toXML(this.tokenParams);
		Preconditions.checkState(Arguments.notEmpty(xml), "alipay.mobile.token.empty");
		xml = CharMatcher.JAVA_ISO_CONTROL.removeFrom(xml);
		xml = CharMatcher.WHITESPACE.removeFrom(xml);
		this.params.put("req_data", xml);
	}

	public void sign() {
		try {
			this.params.put("sec_id", "MD5");
			String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
			String sign = Hashing.md5().newHasher().putString(toVerify, Charsets.UTF_8)
					.putString(this.alipayToken.getKey(), Charsets.UTF_8).hash().toString();
			this.params.put("sign", sign);
		} catch (Exception var3) {
			throw new RuntimeException(var3);
		}
	}

	public String url() {
		this.buildReqData();
		this.sign();
		String suffix = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
		return this.alipayToken.getWapGateway() + "?" + suffix;
	}

	public String token() {
		String tokenUrl = this.url();
		log.debug("token url: {}", tokenUrl);
		String body = HttpRequest.get(tokenUrl, true, new Object[0]).body();
		log.debug("token body: {}", body);
		String backData = null;

		try {
			backData = URLDecoder.decode(body, "UTF-8");
		} catch (UnsupportedEncodingException var10) {
			log.error("alipay mobile get token fail , cause {}", Throwables.getStackTraceAsString(var10));
			return null;
		}

		TreeMap<String, String> map = this.parseUrlToMap(backData);
		String resErrorXml = (String) map.get("res_error");
		if (!Strings.isNullOrEmpty(resErrorXml)) {
			Map<?, ?> errorMap = (Map<?, ?>) errorxStream.fromXML(resErrorXml);
			log.error("alipay mobile pay get token fail, error map: {}", errorMap);
			return null;
		} else {
			boolean signVerify = this.verifyMap(map);
			Preconditions.checkState(signVerify, "alipay.mobile.token.sign.verify.fail");
			String resDataXml = (String) map.get("res_data");
			Map<?, ?> dataMap = (Map<?, ?>) dataxStream.fromXML(resDataXml);
			String requestToken = dataMap.get("request_token").toString();
			return requestToken;
		}
	}

	private TreeMap<String, String> parseUrlToMap(String url) {
		TreeMap<String, String> map = Maps.newTreeMap();
		Matcher matcher = pattern.matcher("&" + url);
		if (matcher.find()) {
			matcher.reset();

			while (matcher.find()) {
				map.put(matcher.group(2), matcher.group(3));
			}
		} else {
			log.warn("[ERROR] NOT FOUND!");
		}

		return map;
	}

	private boolean verifyMap(TreeMap<String, String> map) {
		Map<String, String> mapVerify = Maps.newTreeMap(map);
		String sign = (String) mapVerify.get("sign");
		mapVerify.remove("sign");
		return verify(mapVerify, sign, this.alipayToken);
	}

	static {
		mapxStream.autodetectAnnotations(true);
		mapxStream.registerConverter(new PojoMapConverter());
		mapxStream.alias("direct_trade_create_req", TreeMap.class);
		errorxStream.autodetectAnnotations(true);
		errorxStream.registerConverter(new PojoMapConverter());
		errorxStream.alias("error", TreeMap.class);
		dataxStream.autodetectAnnotations(true);
		dataxStream.registerConverter(new PojoMapConverter());
		dataxStream.alias("direct_trade_create_res", TreeMap.class);
	}
}
