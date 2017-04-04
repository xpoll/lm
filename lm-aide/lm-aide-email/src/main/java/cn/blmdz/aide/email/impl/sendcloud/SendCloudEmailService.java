package cn.blmdz.aide.email.impl.sendcloud;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Lists;

import cn.blmdz.aide.email.EmailException;
import cn.blmdz.aide.email.EmailService;
import cn.blmdz.aide.email.util.StringHelper;
import cn.blmdz.home.common.util.JsonMapper;

public class SendCloudEmailService implements EmailService {
	private static final Logger log = LoggerFactory.getLogger(SendCloudEmailService.class);
	protected final SendCloudToken token;
	private final JsonMapper JSON_MAPPER;
	protected final ExecutorService emailExecutor;

	public SendCloudEmailService(String user, String triggerUser, String batchUser, String key, String gateway) {
		this.JSON_MAPPER = JsonMapper.nonDefaultMapper();
		this.token = new SendCloudToken();
		this.token.setUser(user);
		this.token.setTriggerUser(triggerUser);
		this.token.setBatchUser(batchUser);
		this.token.setKey(key);
		this.token.setGateway(gateway);
		this.emailExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() + 1);
	}

	public SendCloudEmailService(SendCloudToken token) {
		this(token.getUser(), token.getTriggerUser(), token.getBatchUser(), token.getKey(), token.getGateway());
	}

	public String send(String subject, String content, String toes, String attachments) {
		toes = StringHelper.singleString2JsonFormat(toes);
		List<String> emails = JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(toes,
				JsonMapper.JSON_NON_EMPTY_MAPPER.createCollectionType(List.class, new Class[] { String.class }));
		if (emails.size() > 1) {
			for (String email : emails) {
				this.doSendMail(subject, content, email, true);
			}
		} else {
			this.doSendMail(subject, content, (String) emails.get(0), false);
		}

		return Boolean.TRUE.toString();
	}

	private Boolean doSendMail(String subject, String content, String to, boolean batch) {
		String url = this.token.getGateway();
		HttpClient httpclient = HttpClientBuilder.create().build();
		HttpPost httpost = new HttpPost(url);
		List<BasicNameValuePair> nvps = Lists.newArrayList();
		if (batch) {
			nvps.add(new BasicNameValuePair("api_user", this.token.getBatchUser()));
		} else {
			nvps.add(new BasicNameValuePair("api_user", this.token.getTriggerUser()));
		}

		nvps.add(new BasicNameValuePair("api_key", this.token.getKey()));
		nvps.add(new BasicNameValuePair("from", this.token.getUser()));
		nvps.add(new BasicNameValuePair("to", to));
		nvps.add(new BasicNameValuePair("subject", subject));
		nvps.add(new BasicNameValuePair("html", content));

		try {
			httpost.setEntity(new UrlEncodedFormEntity(nvps, "UTF-8"));
		} catch (Exception var14) {
			throw new EmailException("make.form.request.params.failed");
		}

		HttpResponse response;
		try {
			response = httpclient.execute(httpost);
		} catch (Exception var13) {
			throw new EmailException("http.post.failed");
		}

		if (response.getStatusLine().getStatusCode() == 200) {
			String result;
			try {
				result = EntityUtils.toString(response.getEntity());
			} catch (Exception var12) {
				throw new EmailException("parse.response.xml.failed");
			}

			@SuppressWarnings("unchecked")
			Map<String, List<String>> mappedRes = this.JSON_MAPPER.fromJson(result, Map.class);
			if (!mappedRes.get("message").equals("success")) {
				log.error("failed to send email(subject={}, content={}, to={}), cause: {}",
						new Object[] { subject, content, to, mappedRes.get("errors").get(0) });
				throw new EmailException(result);
			} else {
				return Boolean.TRUE;
			}
		} else {
			log.error("failed to send email(subject={}, content={}, to={}), cause status code: {}",
					new Object[] { subject, content, to, Integer.valueOf(response.getStatusLine().getStatusCode()) });
			throw new EmailException("email.send.fail." + response.getStatusLine().getStatusCode());
		}
	}
}
