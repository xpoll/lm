package cn.blmdz.aide.pay.channel.kjtpay.util;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;
import com.itrus.cryptorole.CryptoException;
import com.itrus.cryptorole.NotSupportException;
import com.itrus.cryptorole.bc.RecipientBcImpl;
import com.itrus.cryptorole.bc.SenderBcImpl;
import com.itrus.util.Base64;

public class Core {
	public static SenderBcImpl sender = new SenderBcImpl();
	public static RecipientBcImpl recipient = new RecipientBcImpl();
	public static String pfxFileName = "/home/uxms/kjt_data/pay.pfx";
	public static String certFileName = "/home/uxms/kjt_data/CVM/cafiles/pd.cer";
	public static String keyPassword = "12345";

	public static Map<String, String> paraFilter(Map<String, String> sArray) {
		Map<String, String> result = Maps.newHashMap();
		if (sArray != null && sArray.size() > 0) {
			for (String key : sArray.keySet()) {
				String value = (String) sArray.get(key);
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

	public static Map<String, String> encode(Map<String, String> sArray) {
		Map<String, String> result = Maps.newHashMap();
		if (sArray != null && sArray.size() > 0) {
			String charset = (String) sArray.get("_input_charset");

			for (String key : sArray.keySet()) {
				String value = (String) sArray.get(key);
				if (value != null && !value.equals("")) {
					try {
						value = URLEncoder.encode(value, charset);
					} catch (UnsupportedEncodingException var7) {
						var7.printStackTrace();
					}
				}

				result.put(key, value);
			}

			return result;
		} else {
			return result;
		}
	}

	public static String createLinkString(Map<String, String> params, boolean encode) {
		List<String> keys = new ArrayList<String>(params.keySet());
		Collections.sort(keys);
		String prestr = "";
		String charset = (String) params.get("_input_charset");

		for (int i = 0; i < keys.size(); ++i) {
			String key = (String) keys.get(i);
			String value = (String) params.get(key);
			if (encode) {
				try {
					value = URLEncoder.encode(value, charset);
				} catch (UnsupportedEncodingException var9) {
					var9.printStackTrace();
				}
			}

			if (i == keys.size() - 1) {
				prestr = prestr + key + "=" + value;
			} else {
				prestr = prestr + key + "=" + value + "&";
			}
		}

		return prestr;
	}

	public static String buildRequest(Map<String, String> sPara) throws Exception {
		String prestr = createLinkString(sPara, false);
		String mysign = sender.signMessage(prestr);
		return mysign;
	}

	public static Map<String, String> buildRequestPara(Map<String, String> sParaTemp, String signType, String key,
			String inputCharset) throws Exception {
		Map<String, String> sPara = paraFilter(sParaTemp);
		if (Strings.isNullOrEmpty(signType)) {
			return sPara;
		} else {
			String mysign = buildRequest(sPara);
			sPara.put("sign", mysign);
			sPara.put("sign_type", signType);
			return encode(sPara);
		}
	}

	public static String encryptData(String oriMessage, String inputCharset) {
		String str = null;
		byte[] encryMsg = null;

		try {
			encryMsg = sender.encryptMessage(oriMessage.getBytes(inputCharset));
		} catch (UnsupportedEncodingException var5) {
			var5.printStackTrace();
		} catch (CryptoException var6) {
			var6.printStackTrace();
		}

		str = Base64.encode(encryMsg);
		return str;
	}

	public static String decryptData(String oriMessage, String inputCharset) {
		String str = null;
		byte[] decryMsg = null;

		try {
			decryMsg = recipient.decryptMessage(Base64.decode(oriMessage.getBytes(inputCharset)));
			str = new String(decryMsg, "UTF-8");
		} catch (UnsupportedEncodingException var5) {
			var5.printStackTrace();
		} catch (CryptoException var6) {
			var6.printStackTrace();
		}

		return str;
	}

	static {
		try {
			sender.initCertWithKey(pfxFileName, keyPassword);
			recipient.initCertWithKey(pfxFileName, keyPassword);
			InputStream streamCert = new FileInputStream(certFileName);
			CertificateFactory factory = CertificateFactory.getInstance("X.509");
			X509Certificate X509Cert = (X509Certificate) factory.generateCertificate(streamCert);
			sender.addRecipientCert(X509Cert);
		} catch (NotSupportException var3) {
			var3.printStackTrace();
		} catch (CryptoException var4) {
			var4.printStackTrace();
		} catch (FileNotFoundException var5) {
			var5.printStackTrace();
		} catch (CertificateException var6) {
			var6.printStackTrace();
		}

	}
}
