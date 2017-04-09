package cn.blmdz.wolf.web.msg.config;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.aide.apppush.AppPushService;
import cn.blmdz.aide.apppush.impl.umeng.UmengAppPushService;
import cn.blmdz.aide.apppush.impl.umeng.UmengToken;
import cn.blmdz.aide.email.EmailService;
import cn.blmdz.aide.email.impl.sendcloud.SendCloudEmailService;
import cn.blmdz.aide.email.impl.sendcloud.SendCloudToken;
import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.aide.sms.impl.alibaba.AliSmsService;
import cn.blmdz.aide.sms.impl.alibaba.AliSmsToken;
import cn.blmdz.wolf.config.ConfigCenter;
import cn.blmdz.wolf.msg.component.MessagePostHandler;
import cn.blmdz.wolf.msg.component.MessagePreHandler;
import cn.blmdz.wolf.msg.component.MessageValidatorChain;
import cn.blmdz.wolf.msg.component.handler.DefaultMessagePostHandler;
import cn.blmdz.wolf.msg.component.handler.DefaultMessagePreHandler;
import cn.blmdz.wolf.msg.component.validate.DefaultMessageValidatorChain;
import cn.blmdz.wolf.web.msg.MsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.MsgWebService;
import cn.blmdz.wolf.web.msg.impl.SmartMsgWebService;
import cn.blmdz.wolf.web.msg.impl.common.DefaultMsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.mock.MockAppPushService;
import cn.blmdz.wolf.web.msg.mock.MockEmailService;
import cn.blmdz.wolf.web.msg.mock.MockMsgGatewayBuilder;
import cn.blmdz.wolf.web.msg.mock.MockSmsService;
import cn.blmdz.wolf.web.msg.test.AppPushController;
import cn.blmdz.wolf.web.msg.test.EmailController;
import cn.blmdz.wolf.web.msg.test.NotifyController;
import cn.blmdz.wolf.web.msg.test.SmartMsgController;
import cn.blmdz.wolf.web.msg.test.SmsController;

@Configuration
public class MsgWebConfig {
	@Bean
	public SmartMsgWebService smartMsgWebService() {
		return new SmartMsgWebService();
	}

	@Configuration
	@ConditionalOnBean({ MsgGatewayBuilder.class })
	@ConditionalOnProperty(name = { "msg.webservice.version" }, havingValue = "db")
	public static class DbMsgWebServiceConfig {
		@Configuration
		@ConditionalOnProperty(name = { "msg.webservice.channel.apppush" })
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.apppush.common",
				"cn.blmdz.wolf.web.msg.impl.apppush.db" })
		public static class DbAppPushConfig {
		}

		@Configuration
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.common.db" })
		public static class DbCommonConfig {
		}

		@Configuration
		@ConditionalOnProperty(name = { "msg.webservice.channel.email" })
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.email.common", "cn.blmdz.wolf.web.msg.impl.email.db" })
		public class DbEmailConfig {
		}

		@Configuration
		@ConditionalOnProperty(name = { "msg.webservice.channel.notify" })
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.notify.common", "cn.blmdz.wolf.web.msg.impl.notify.db",
				"cn.blmdz.wolf.web.msg.impl.notify.controller" })
		public class DbNotifyConfig {
		}

		@Configuration
		@ConditionalOnProperty(name = { "msg.webservice.channel.sms" })
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.sms.common", "cn.blmdz.wolf.web.msg.impl.sms.db" })
		public class DbSmsConfig {
		}
	}

	@Configuration
	public static class MsgGateWayConfig {
		@ConditionalOnProperty(name = { "msg.gateway.version" }, havingValue = "db")
		public static class DbMsgGatewayConfig {
			@Bean
			public MsgGatewayBuilder dbMsgGatewayBuilder() {
				return new DefaultMsgGatewayBuilder();
			}

			@Bean
			@ConditionalOnProperty(name = { "msg.current.smsService" }, havingValue = "aliSmsService")
			public SmsService aliSmsService(ConfigCenter configCenter) {
				AliSmsToken token = new AliSmsToken();
				token.setAppKey((String) configCenter.get("msg.alisms.appKey").or("appKey"));
				token.setAppSecret((String) configCenter.get("msg.alisms.appSecret").or("appSecret"));
				token.setSmsUrl((String) configCenter.get("msg.alisms.url").or("http://gw.api.taobao.com/router/rest"));
				token.setVersion((String) configCenter.get("msg.alisms.version").or("2.0"));
				return new AliSmsService(token);
			}

			@Bean
			@ConditionalOnProperty(name = { "msg.current.emailService" }, havingValue = "sendCloudEmailService")
			public EmailService sendCloudEmailService(ConfigCenter configCenter) {
				SendCloudToken token = new SendCloudToken();
				token.setBatchUser((String) configCenter.get("msg.sendcloud.batchUser").or("batchUser"));
				token.setUser((String) configCenter.get("msg.sendcloud.user").or("user"));
				token.setTriggerUser((String) configCenter.get("msg.sendcloud.triggerUser").or("triggerUser"));
				token.setGateway((String) configCenter.get("msg.sendcloud.gateway")
						.or("http://sendcloud.sohu.com/webapi/mail.send.json"));
				token.setKey((String) configCenter.get("msg.sendcloud.key").or("key"));
				return new SendCloudEmailService(token);
			}

			@Bean
			@ConditionalOnProperty(name = { "msg.current.appPushService" }, havingValue = "umengAppPushService")
			public AppPushService umengAppPushServiceAndroid(ConfigCenter configCenter) {
				UmengToken token = new UmengToken();
				token.setAppKey((String) configCenter.get("msg.umeng.android.appKey").or("defaultKey"));
				token.setAppMasterSecret((String) configCenter.get("msg.umeng.android.appSecret").or("defaultSecret"));
				token.setSendUrl((String) configCenter.get("msg.umeng.sendUrl").or("http://msg.umeng.com/api/send"));
				return new UmengAppPushService(token);
			}

			@Bean
			@ConditionalOnProperty(name = { "msg.current.appPushService" }, havingValue = "umengAppPushService")
			public AppPushService umengAppPushServiceIos(ConfigCenter configCenter) {
				UmengToken token = new UmengToken();
				token.setAppKey((String) configCenter.get("msg.umeng.ios.appKey").or("defaultKey"));
				token.setAppMasterSecret((String) configCenter.get("msg.umeng.ios.appSecret").or("defaultSecret"));
				token.setSendUrl((String) configCenter.get("msg.umeng.sendUrl").or("http://msg.umeng.com/api/send"));
				return new UmengAppPushService(token);
			}
		}

		@ConditionalOnProperty(name = { "msg.gateway.version" }, havingValue = "mock")
		public static class MockMsgGatewayConfig {
			@Bean
			public MsgGatewayBuilder mockMsgGatewayBuilder() {
				return new MockMsgGatewayBuilder();
			}

			@Bean
			public SmsService mockSmsService() {
				return new MockSmsService();
			}

			@Bean
			public EmailService mockEmailService() {
				return new MockEmailService();
			}

			@Bean
			public AppPushService mockAppPushService() {
				return new MockAppPushService();
			}
		}

		@ConditionalOnProperty(name = { "msg.gateway.version" }, havingValue = "simple", matchIfMissing = true)
		public static class SimpleMsgGatewayConfig {
			@Bean
			public MsgGatewayBuilder simpleMsgGatewayBuilder() {
				return new DefaultMsgGatewayBuilder();
			}

			@Bean
			@ConditionalOnProperty(name = { "msg.current.smsService" }, havingValue = "aliSmsService")
			public SmsService aliSmsService(@Value("${msg.alisms.appKey:defaultKey}") String appKey,
					@Value("${msg.alisms.appSecret:defaultSecret}") String appSecret,
					@Value("${msg.alisms.smsUrl:http://gw.api.taobao.com/router/rest}") String smsUrl,
					@Value("${msg.alisms.version:2.0}") String version) {
				return new AliSmsService(appKey, appSecret, smsUrl, version);
			}

			@Bean
			@ConditionalOnProperty(name = { "msg.current.emailService" }, havingValue = "sendCloudEmailService")
			public EmailService sendCloudEmailService(@Value("${msg.sendcloud.user:user}") String user,
					@Value("${msg.sendcloud.triggerUser:triggerUser}") String triggerUser,
					@Value("${msg.sendcloud.batchUser:batchUser}") String batchUser,
					@Value("${msg.sendcloud.key:defaultkey}") String key,
					@Value("${msg.sendcloud.gateway:gateway}") String gateway) {
				return new SendCloudEmailService(user, triggerUser, batchUser, key, gateway);
			}

			@Bean
			@ConditionalOnProperty(name = { "msg.current.appPushService" }, havingValue = "umengAppPushService")
			public AppPushService umengAppPushServiceAndroid(
					@Value("${msg.umeng.android.appKey:defaultKey}") String appKey,
					@Value("${msg.umeng.android.appSecret:defaultSecret}") String appSecret) {
				return new UmengAppPushService(appKey, appSecret);
			}

			@Bean
			@ConditionalOnProperty(name = { "msg.current.appPushService" }, havingValue = "umengAppPushService")
			public AppPushService umengAppPushServiceIos(@Value("${msg.umeng.ios.appKey:defaultKey}") String appKey,
					@Value("${msg.umeng.ios.appSecret:defaultSecret}") String appSecret) {
				return new UmengAppPushService(appKey, appSecret);
			}
		}
	}

	@Configuration
	@ConditionalOnBean({ MsgGatewayBuilder.class })
	@ConditionalOnProperty(name = { "msg.webservice.version" }, havingValue = "simple")
	public static class SimpleMsgWebServiceConfig {
		@Configuration
		@ConditionalOnProperty(name = { "msg.webservice.channel.apppush" })
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.apppush.common",
				"cn.blmdz.wolf.web.msg.impl.apppush.simple" })
		public class SimpleAppPushConfig {
		}

		@Configuration
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.common.simple" })
		public class SimpleCommonConfig {
			@Bean
			@ConditionalOnMissingBean({ MessageValidatorChain.class })
			public MessageValidatorChain messageValidatorChain() {
				return new DefaultMessageValidatorChain();
			}

			@Bean
			@ConditionalOnMissingBean({ MessagePreHandler.class })
			public MessagePreHandler messagePreHandler() {
				return new DefaultMessagePreHandler();
			}

			@Bean
			@ConditionalOnMissingBean({ MessagePostHandler.class })
			public MessagePostHandler messagePostHandler() {
				return new DefaultMessagePostHandler();
			}
		}

		@Configuration
		@ConditionalOnProperty(name = { "msg.webservice.channel.email" })
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.email.common",
				"cn.blmdz.wolf.web.msg.impl.email.simple" })
		public class SimpleEmailConfig {
		}

		@Configuration
		@ConditionalOnProperty(name = { "msg.webservice.channel.notify" })
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.notify.common",
				"cn.blmdz.wolf.web.msg.impl.notify.controller", "cn.blmdz.wolf.web.msg.impl.notify.simple" })
		public class SimpleNotifyConfig {
		}

		@Configuration
		@ConditionalOnProperty(name = { "msg.webservice.channel.sms" })
		@ComponentScan({ "cn.blmdz.wolf.web.msg.impl.sms.common", "cn.blmdz.wolf.web.msg.impl.sms.simple" })
		public class SimpleSmsConfig {
		}
	}

	@Configuration
	@ConditionalOnProperty(name = { "msg.test.enabled" }, havingValue = "true")
	public static class TestMsgWebServiceConfig {
		@Bean
		@ConditionalOnBean(name = { "appPushWebService" })
		@ConditionalOnProperty(name = { "msg.webservice.channel.apppush" })
		public AppPushController appPushController(@Qualifier("appPushWebService") MsgWebService appPushWebService) {
			return new AppPushController(appPushWebService);
		}

		@Bean
		@ConditionalOnBean(name = { "emailWebService" })
		@ConditionalOnProperty(name = { "msg.webservice.channel.email" })
		public EmailController emailController(@Qualifier("emailWebService") MsgWebService emailWebService) {
			return new EmailController(emailWebService);
		}

		@Bean
		@ConditionalOnBean(name = { "notifyWebService" })
		@ConditionalOnProperty(name = { "msg.webservice.channel.notify" })
		public NotifyController notifyController(@Qualifier("notifyWebService") MsgWebService notifyWebService) {
			return new NotifyController(notifyWebService);
		}

		@Bean
		@ConditionalOnBean(name = { "smsWebService" })
		@ConditionalOnProperty(name = { "msg.webservice.channel.sms" })
		public SmsController smsController(@Qualifier("smsWebService") MsgWebService smsWebService) {
			return new SmsController(smsWebService);
		}

		@Bean
		public SmartMsgController smartMsgController(SmartMsgWebService smartMsgWebService) {
			return new SmartMsgController(smartMsgWebService);
		}
	}
}
