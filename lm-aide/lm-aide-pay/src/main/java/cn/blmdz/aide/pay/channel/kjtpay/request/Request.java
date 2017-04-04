package cn.blmdz.aide.pay.channel.kjtpay.request;

import java.io.FileInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.text.DecimalFormat;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Joiner;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.itrus.cryptorole.bc.SenderBcImpl;

import cn.blmdz.home.common.util.Arguments;

public class Request {
	private static final Logger log = LoggerFactory.getLogger(Request.class);
	protected static final String VERSION = "1.0";
	protected static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("0.##");
	protected Map<String, String> params = Maps.newTreeMap();
	protected KjtToken kjtToken;

	protected Request(KjtToken kjtToken) {
		this.params.put("partner_id", kjtToken.getPid());
		this.params.put("version", "1.0");
		this.params.put("_input_charset", "UTF-8");
		this.kjtToken = kjtToken;
	}

	public Map<String, String> param() {
		return this.params;
	}

	public String url() {
		this.sign();
		String suffix = Joiner.on('&').withKeyValueSeparator("=").join(this.params);
		return this.kjtToken.getGateway() + "?" + suffix;
	}

	public void sign() {
		try {
			Map<String, String> param = paraFilter(this.params);
			String toVerify = Joiner.on('&').withKeyValueSeparator("=").join(param);
			SenderBcImpl send = new SenderBcImpl();
			send.initCertWithKey(this.kjtToken.getPfxPath(), this.kjtToken.getKeyPassword());
			SenderBcImpl sender = new SenderBcImpl();
			SenderBcImpl recipient = new SenderBcImpl();
			sender.initCertWithKey(this.kjtToken.getPfxPath(), this.kjtToken.getKeyPassword());
			recipient.initCertWithKey(this.kjtToken.getPfxPath(), this.kjtToken.getKeyPassword());
			InputStream streamCert = new FileInputStream(this.kjtToken.getCerPath());
			CertificateFactory factory = CertificateFactory.getInstance("X.509");
			X509Certificate X509Cert = (X509Certificate) factory.generateCertificate(streamCert);
			sender.addRecipientCert(X509Cert);
			String sign = sender.signMessage(toVerify);
			this.params.put("sign", sign);
			this.params.put("sign_type", "ITRUSSRV");
			this.encoderSign();
			this.encoderTradeList();
		} catch (Exception var10) {
			throw new RuntimeException(var10);
		}
	}

	public void encoderTradeList() {
		String content = (String) this.params.get("trade_list");
		if (Arguments.notEmpty(content)) {
			try {
				this.params.put("trade_list", URLEncoder.encode(content, "UTF-8"));
			} catch (UnsupportedEncodingException var3) {
				var3.printStackTrace();
			}
		}

	}

	public void encoderSign() {
		String sign = (String) this.params.get("sign");
		if (Arguments.notEmpty(sign)) {
			try {
				this.params.put("sign", URLEncoder.encode(sign, "UTF-8"));
			} catch (UnsupportedEncodingException var3) {
				var3.printStackTrace();
			}
		}

	}

	public static Map<String, String> paraFilter(Map<String, String> param) {
		Map<String, String> result = Maps.newTreeMap();
		if (param != null && param.size() > 0) {
			for (String key : param.keySet()) {
				String value = param.get(key);
				if (value != null && !value.equals("") && !key.equalsIgnoreCase("sign")
						&& !key.equalsIgnoreCase("sign_type")) {
					result.put(key, value);
				}
			}

			return result;
		} else {
			return result;
		}
	}

	public String request(Map<String, String> params) {
		try {
			HttpRequest request = HttpRequest.post(this.kjtToken.getGateway()).form(params);
			if (!request.ok()) {
				throw new Exception("Kjtpay request post failed");
			} else {
				return request.url().toString();
			}
		} catch (Exception var3) {
			log.error("pay request(params={}) failed, error code={}", params, Throwables.getStackTraceAsString(var3));
			throw new RuntimeException(var3);
		}
	}

	public String mobileUrl() {
		try {
			this.sign();
			HttpRequest request = HttpRequest.post(this.kjtToken.getGateway()).contentType("application/json", "UTF-8")
					.form(this.param());
			if (!request.ok()) {
				throw new Exception("Kjtpay request post failed");
			} else {
				String url = request.body();
				if (!url.contains("is_success=T")) {
					log.info("kjt mobile pay return body:{}", url);
					return "";
				} else {
					return url.substring(url.indexOf("https://m.kjtpay.com"));
				}
			}
		} catch (Exception var3) {
			log.error("pay request(params={}) failed, error code={}", this.params,
					Throwables.getStackTraceAsString(var3));
			throw new RuntimeException(var3);
		}
	}
}
