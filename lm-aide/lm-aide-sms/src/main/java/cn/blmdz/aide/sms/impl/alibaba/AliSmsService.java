package cn.blmdz.aide.sms.impl.alibaba;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.collect.Maps;
import com.google.common.hash.Hashing;

import cn.blmdz.aide.sms.SmsException;
import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.home.common.util.Joiners;
import cn.blmdz.home.common.util.JsonMapper;

public class AliSmsService implements SmsService {
	private static final Logger log = LoggerFactory.getLogger(AliSmsService.class);
	private final AliSmsToken token;

	public AliSmsService(String appKey, String appSecret, String smsUrl, String version) {
		this.token = new AliSmsToken();
		this.token.setAppKey(appKey);
		this.token.setAppSecret(appSecret);
		this.token.setSmsUrl(smsUrl);
		this.token.setVersion(version);
		log.info("alismsToken: {}", this.token);
	}

	public AliSmsService(AliSmsToken aliSmsToken) {
		this(aliSmsToken.getAppKey(), aliSmsToken.getAppSecret(), aliSmsToken.getSmsUrl(), aliSmsToken.getVersion());
	}

	public String send(String from, String toes, String message, String extra) {
		return this.send(from, toes, message);
	}

	public String send(String from, String toes, String message) {
		toes = this.singleString2JsonFormat(toes);
		@SuppressWarnings("unchecked")
		List<String> toList = JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(toes, List.class);
		if (toList.size() > 200) {
			log.error("group count can not be greater than 200");
			throw new SmsException("group.too.long");
		} else {
			AliSmsMessage smsMessage = (AliSmsMessage) JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(message,
					AliSmsMessage.class);
			return this.doSendSms(from, Joiners.COMMA.join(toList), smsMessage.getSmsName(),
					smsMessage.getSmsTemplate(), smsMessage.getSmsParam());
		}
	}

	public Integer available() {
		return null;
	}

	private String doSendSms(String from, String to, String smsName, String smsTemplate, Map<?, ?> smsParam) {
		Map<String, Object> params = Maps.newHashMap();
		params.put("method", "alibaba.aliqin.fc.sms.num.send");
		params.put("app_key", this.token.getAppKey());
		params.put("timestamp", (new DateTime()).toString("yyyy-MM-dd HH:mm:ss"));
		params.put("v", this.token.getVersion());
		params.put("format", "json");
		params.put("sign_method", "md5");
		params.put("sms_type", "normal");
		params.put("sms_free_sign_name", smsName);
		params.put("rec_num", to);
		params.put("sms_param", JsonMapper.nonEmptyMapper().toJson(smsParam));
		params.put("sms_template_code", smsTemplate);
		String sign = smsSign(this.token.getAppSecret(), params);
		params.put("sign", sign);
		String body = HttpRequest.post(this.token.getSmsUrl(), params, true).connectTimeout(30000).body();
		AliSmsSendSuccessResult result = (AliSmsSendSuccessResult) JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(body,
				AliSmsSendSuccessResult.class);
		if (result != null && result.getAlibaba_aliqin_fc_sms_num_send_response() != null) {
			return body;
		} else {
			throw new SmsException(body);
		}
	}

	private static String smsSign(String secret, Map<String, Object> params) {
		Map<String, Object> sortParams = Maps.newTreeMap();

		for (Entry<String, Object> param : params.entrySet()) {
			sortParams.put(param.getKey(), param.getValue());
		}

		String toVerify = Joiner.on("").withKeyValueSeparator("").join(sortParams);
		return Hashing.md5().newHasher(32).putString(secret, Charsets.UTF_8).putString(toVerify, Charsets.UTF_8)
				.putString(secret, Charsets.UTF_8).hash().toString().toUpperCase();
	}

	protected String singleString2JsonFormat(String from) {
		if (!from.contains("[")) {
			from = "[\"" + from + "\"]";
		}
		return from;
	}
}
