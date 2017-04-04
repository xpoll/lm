package cn.blmdz.aide.pay.channel.alipay.request;

import java.util.Map;
import java.util.TreeMap;

import com.google.common.base.CharMatcher;
import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.collect.Maps;
import com.google.common.hash.Hashing;
import com.thoughtworks.xstream.XStream;

import cn.blmdz.aide.pay.driver.CDATAXppDriver;
import cn.blmdz.aide.pay.driver.PojoMapConverter;
import cn.blmdz.home.common.util.Arguments;

public class MobilePayRequest extends Request {
	private static XStream mapxStream = new XStream(new CDATAXppDriver());
	protected Map<String, Object> payParams = Maps.newTreeMap();

	protected MobilePayRequest(AlipayToken alipayToken) {
		super(alipayToken);
		this.params.remove("seller_email");
		this.params.remove("_input_charset");
		this.params.put("service", "alipay.wap.auth.authAndExecute");
		this.params.put("format", "xml");
		this.params.put("v", "2.0");
	}

	public static MobilePayRequest build(AlipayToken alipayToken) {
		return new MobilePayRequest(alipayToken);
	}

	public MobilePayRequest requestToken(String requestToken) {
		if (Arguments.notNull(requestToken)) {
			this.payParams.put("request_token", requestToken);
		}

		return this;
	}

	private void buildReqData() {
		String xml = mapxStream.toXML(this.payParams);
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

	static {
		mapxStream.autodetectAnnotations(true);
		mapxStream.registerConverter(new PojoMapConverter());
		mapxStream.alias("auth_and_execute_req", TreeMap.class);
	}
}
