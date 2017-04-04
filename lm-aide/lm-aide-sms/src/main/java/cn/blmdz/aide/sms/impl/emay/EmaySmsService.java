package cn.blmdz.aide.sms.impl.emay;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Objects;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMap.Builder;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

import cn.blmdz.aide.sms.SmsException;
import cn.blmdz.aide.sms.SmsService;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.util.Joiners;
import cn.blmdz.home.common.util.JsonMapper;

public class EmaySmsService implements SmsService {
	private static final Logger log = LoggerFactory.getLogger(EmaySmsService.class);
	private final EmayToken token = new EmayToken();

	public EmaySmsService(String account, String password, String sendUrl, String queryUrl, String registerUrl) {
		this.token.setAccount(account);
		this.token.setPassword(password);
		this.token.setSendUrl(sendUrl);
		this.token.setQueryUrl(queryUrl);
		this.token.setRegisterUrl(registerUrl);
	}

	public EmaySmsService(EmayToken emayToken) {
		this.token.setRegisterUrl(emayToken.getRegisterUrl());
		this.token.setQueryUrl(emayToken.getQueryUrl());
		this.token.setSendUrl(emayToken.getSendUrl());
		this.token.setPassword(emayToken.getPassword());
		this.token.setAccount(emayToken.getAccount());
	}

	public String send(String from, String toes, String message, String extra) {
		return this.send(from, toes, message);
	}

	public String send(String from, String toes, String message) {
		toes = this.singleString2JsonFormat(toes);
		if (message.length() > 500) {
			log.error("message too long: should less than 500, actual={}", Integer.valueOf(message.length()));
			throw new SmsException("message.too.long");
		} else {
			@SuppressWarnings("unchecked")
			List<String> toList = JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(toes, List.class);
			if (toList.size() > 200) {
				log.error("group count can not be greater than 200");
				throw new SmsException("group.too.long");
			} else {
				return this.doSendSms(from, Joiners.COMMA.join(toList), message);
			}
		}
	}

	public Integer available() {
		Map<String, String> params = ImmutableMap.of("cdkey", this.token.getAccount(), "password",
				this.token.getPassword());
		String body = HttpRequest.get(this.token.getQueryUrl(), params, false).body();
		@SuppressWarnings("unchecked")
		Map<String, String> result = (Map<String, String>) EmaySmsService.XMLMapper.fromXML(body);
		return Integer.valueOf(Integer.parseInt((String) result.get("message")));
	}

	private String doSendSms(String from, String to, String message) {
		String seqId = to + "_" + UUID.randomUUID().toString();
		Map<String, String> params = new Builder<String, String>()
				.put("cdkey", this.token.getAccount())
				.put("password", this.token.getPassword())
				.put("phone", to)
				.put("message", message)
				.put("seqid", seqId)
				.put("smspriority", "3")
				.build();
		String body = HttpRequest.get(this.token.getSendUrl(), params, true).connectTimeout(30000).body();
		@SuppressWarnings("unchecked")
		Map<String, String> result = (Map<String, String>) EmaySmsService.XMLMapper.fromXML(body.trim());
		if (!Objects.equal(result.get("error"), "0")) {
			log.error("failed to send message from {} to {},error code:{}",
					new Object[] { from, to, result.get("error") });
			throw new ServiceException("emay.sms.error.code." + (String) result.get("error"));
		} else {
			return body;
		}
	}

	protected String singleString2JsonFormat(String from) {
		if (!from.contains("[")) {
			from = "[\"" + from + "\"]";
		}

		return from;
	}

	public static final class XMLMapper {
		private static final XStream xstream = new XStream();

		public static Object fromXML(String xml) {
			return xstream.fromXML(xml);
		}

		static {
			XMLMapper.MapEntryConverter converter = new XMLMapper.MapEntryConverter();
			xstream.aliasType("response", Map.class);
			xstream.registerConverter(converter);
		}

		public static class MapEntryConverter implements Converter {
			@Override
			@SuppressWarnings("rawtypes")
			public boolean canConvert(Class clazz) {
				return Map.class.isAssignableFrom(clazz);
			}
			@Override
			public void marshal(Object value, HierarchicalStreamWriter writer, MarshallingContext context) {
				@SuppressWarnings("unchecked")
				Map<String, String> map = (Map<String, String>) value;

				for (Entry<String, String> entry : map.entrySet()) {
					writer.startNode((String) entry.getKey());
					writer.setValue((String) entry.getValue());
					writer.endNode();
				}

			}
			@Override
			public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
				Builder<String, String> builder = ImmutableMap.builder();

				while (reader.hasMoreChildren()) {
					reader.moveDown();
					builder.put(reader.getNodeName(), reader.getValue());
					reader.moveUp();
				}

				return builder.build();
			}
		}
	}
}
