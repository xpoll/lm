package cn.blmdz.aide.pay.channel.alipay.request;

import java.text.DecimalFormat;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.collect.Maps;
import com.google.common.hash.Hashing;
import com.thoughtworks.xstream.XStream;

public class Request {
	private static final Logger log = LoggerFactory.getLogger(Request.class);
	protected static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("0.##");
	protected Map<String, Object> params = Maps.newTreeMap();
	protected AlipayToken alipayToken;
	private static XStream xStream = new XStream();

	protected Request(AlipayToken alipayToken) {
		this.params.put("partner", alipayToken.getPid());
		if (!Strings.isNullOrEmpty(alipayToken.getAccount())) {
			this.params.put("seller_email", alipayToken.getAccount());
		}

		this.params.put("_input_charset", "utf-8");
		this.alipayToken = alipayToken;
	}

	public Map<String, Object> param() {
		return this.params;
	}

	public String url() {
		this.sign();
		String suffix = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
		return this.alipayToken.getGateway() + "?" + suffix;
	}

	protected Boolean convertToResponse(String body) {
		Preconditions.checkState(!Strings.isNullOrEmpty(body), "alipay.refund.fail");
		AlipaySyncResponse refundResponse = (AlipaySyncResponse) xStream.fromXML(body);
		if (refundResponse.isSuccess()) {
			return Boolean.TRUE;
		} else {
			log.error("refund raise fail: {}", refundResponse.getError());
			return Boolean.FALSE;
		}
	}

	public void sign() {
		try {
			String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
			String sign = Hashing.md5().newHasher().putString(toVerify, Charsets.UTF_8)
					.putString(this.alipayToken.getKey(), Charsets.UTF_8).hash().toString();
			this.params.put("sign", sign);
			this.params.put("sign_type", "MD5");
		} catch (Exception var3) {
			throw new RuntimeException(var3);
		}
	}

	public static boolean verify(Map<String, String> params, String sign, AlipayToken alipayToken) {
		String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(params);
		String expect = Hashing.md5().newHasher().putString(toVerify, Charsets.UTF_8)
				.putString(alipayToken.getKey(), Charsets.UTF_8).hash().toString();
		boolean isSignMatch = Objects.equal(expect, sign);
		if (!isSignMatch) {
			log.error("alipay sign mismatch, expected ({}), actual({}), toVerify is:{}",
					new Object[] { expect, sign, toVerify });
		} else {
			log.info("alipay sign matched, expected ({}), actual({}), toVerify is:{}", expect, sign);
		}

		return isSignMatch;
	}

	@SuppressWarnings("unchecked")
	public static <T> T parse(String xml, Class<T> klass) {
		XStream xs = new XStream();
		xs.autodetectAnnotations(true);
		xs.ignoreUnknownElements();
		xs.processAnnotations(klass);
		return (T) xs.fromXML(xml);
	}

	static {
		xStream.autodetectAnnotations(true);
		xStream.processAnnotations(AlipaySyncResponse.class);
	}
}
