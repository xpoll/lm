package cn.blmdz.aide.apppush.impl.umeng;

import java.util.List;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Charsets;
import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.hash.Hashing;

import cn.blmdz.aide.apppush.AppPushException;
import cn.blmdz.aide.apppush.AppPushService;
import cn.blmdz.aide.apppush.DeviceType;
import cn.blmdz.aide.apppush.impl.umeng.UmengMessageIos.Aps;
import cn.blmdz.aide.apppush.util.StringHelper;
import cn.blmdz.home.common.util.Joiners;
import cn.blmdz.home.common.util.JsonMapper;

public class UmengAppPushService implements AppPushService {
	private final UmengToken token;

	public UmengAppPushService(String appkey, String appMasterSecret) {
		this.token = new UmengToken();
		this.token.setAppKey(appkey);
		this.token.setAppMasterSecret(appMasterSecret);
	}

	public UmengAppPushService(UmengToken umengToken) {
		this(umengToken.getAppKey(), umengToken.getAppMasterSecret());
	}

	public String send(String deviceType, String deviceTokens, String title, String content) {
		if (Strings.isNullOrEmpty(deviceType)) {
			throw new AppPushException("param.device.type.required");
		} else if (Strings.isNullOrEmpty(deviceTokens)) {
			throw new AppPushException("param.device.tokens.required");
		} else if (Strings.isNullOrEmpty(content)) {
			throw new AppPushException("param.content.required");
		} else {
			String toes = StringHelper.singleString2JsonFormat(deviceTokens);
			List<String> toList = JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(toes,
					JsonMapper.JSON_NON_EMPTY_MAPPER.createCollectionType(List.class, new Class[] { String.class }));
			if (toList != null && toList.size() >= 1) {
				UmengMessage message = null;
				if (Objects.equal(deviceType, DeviceType.Android)) {
					message = (UmengMessage) JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(content,
							UmengMessageAndroid.class);
					if (message == null) {
						UmengMessageAndroid androidMessage = new UmengMessageAndroid();
						UmengMessageAndroid.AndroidPayload.Body body = androidMessage.getPayload().getBody();
						body.setTitle(title);
						body.setTicker(title);
						body.setText(content);
						message = androidMessage;
					}
				} else {
					if (!Objects.equal(deviceType, DeviceType.Ios)) {
						throw new AppPushException("device.type.not.supported." + deviceType);
					}

					message = (UmengMessage) JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(content, UmengMessageIos.class);
					if (message == null) {
						UmengMessageIos iosMessage = new UmengMessageIos();
						Aps aps = iosMessage.getPayload().get("aps");
						String alertText = "";
						if (title != null) {
							alertText = alertText + title;
						}

						alertText = alertText + "\n" + content;
						aps.setAlert(alertText);
						message = iosMessage;
					}
				}

				message.setAppkey(this.token.getAppKey());
				message.setDevice_tokens(Joiners.COMMA.join(toList));
				if (toList.size() > 1) {
					message.setType("listcast");
				} else {
					message.setType("unicast");
				}

				String postBody = JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(message);
				String sign = umengSign(this.token.getAppMasterSecret(), this.token.getSendUrl(), postBody);
				String url = this.token.getSendUrl() + "?sign=" + sign;
				String body = HttpRequest.post(url).send(postBody).body();
				UmengResultSend resultSend = (UmengResultSend) JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(body,
						UmengResultSend.class);
				if (!resultSend.isOk().booleanValue()) {
					throw new AppPushException(resultSend.getData().getError_code());
				} else {
					return JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(resultSend.getData());
				}
			} else {
				throw new AppPushException("device.token.required");
			}
		}
	}

	private static String umengSign(String secret, String url, String postBody) {
		return Hashing.md5().newHasher().putString("POST", Charsets.UTF_8).putString(url, Charsets.UTF_8)
				.putString(postBody, Charsets.UTF_8).putString(secret, Charsets.UTF_8).hash().toString().toLowerCase();
	}
}
