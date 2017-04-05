package cn.blmdz.aide.pay.channel.wechatpay;

import com.google.common.collect.Maps;
import java.util.Map;

public class OpenIdUtil {
	private static ThreadLocal<String> openIdThreadLocal = new ThreadLocal<String>();
	private static ThreadLocal<Map<Long, String>> openIdSThreadLocal = new ThreadLocal<Map<Long, String>>();

	public static void putOpenId(Long userId, String openId) {
		Map<Long, String> openIdMap = openIdSThreadLocal.get();
		if (openIdMap == null) {
			openIdMap = Maps.newHashMap();
		}
		openIdMap.put(userId, openId);
		openIdSThreadLocal.set(openIdMap);
	}

	public static void putOpenId(String openId) {
		openIdThreadLocal.set(openId);
	}

	public static String getOpenId(Long userId) {
		return openIdSThreadLocal.get().get(userId);
	}

	public static String getOpenId() {
		return openIdThreadLocal.get();
	}

	public static void clearOpenId(Long userId) {
		Map<Long, String> openIdMap = openIdSThreadLocal.get();
		if (openIdMap != null && !openIdMap.isEmpty()) {
			openIdMap.remove(userId);
		}

	}

	public static void clearOpenId() {
		openIdSThreadLocal.remove();
	}
}
