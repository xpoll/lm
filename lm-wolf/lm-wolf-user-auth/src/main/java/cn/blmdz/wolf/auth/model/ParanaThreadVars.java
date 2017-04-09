package cn.blmdz.wolf.auth.model;

import java.util.Locale;
import java.util.Map;

import com.google.common.base.MoreObjects;
import com.google.common.collect.Maps;

public enum ParanaThreadVars {
	APP,

	DOMAIN,

	HOST,

	LOCALE,

	SEO;

	private static App app;
	private static Map<ParanaThreadVars, Object> noPampasMap;
	private static ThreadLocal<Map<ParanaThreadVars, Object>> currentEnv;

	public static void initNoPampasApp(App initAPP) {
		app = initAPP;
		noPampasMap.put(APP, initAPP);
	}

	private static <T> T get(ParanaThreadVars var) {
		if (currentEnv.get().get(var) == null) {
			return (T) noPampasMap.get(var);
		}

		return (T) currentEnv.get().get(var);
	}

	private static void set(ParanaThreadVars var, Object value) {
		currentEnv.get().put(var, value);
	}

	private static void clear(ParanaThreadVars var) {
		currentEnv.get().remove(var);
	}

	public static void clearAll() {
		currentEnv.get().clear();
	}

	public static void setApp(App app) {
		set(APP, app);
	}

	public static App getApp() {
		return (App) get(APP);
	}

	public static void clearApp() {
		clear(APP);
	}

	public static String getAppKey() {
		return getApp().getKey();
	}

	public static void setDomain(String domain) {
		set(DOMAIN, domain);
	}

	public static String getDomain() {
		return (String) MoreObjects.firstNonNull(get(DOMAIN), getApp().getDomain());
	}

	public static void clearDomain() {
		clear(DOMAIN);
	}

	public static void setHost(String host) {
		set(HOST, host);
	}

	public static String getHost() {
		return (String) get(HOST);
	}

	public static void clearHost() {
		clear(HOST);
	}

	public static void setLocale(Locale locale) {
		set(LOCALE, locale);
	}

	public static Locale getLocale() {
		return (Locale) MoreObjects.firstNonNull(get(LOCALE), Locale.getDefault());
	}

	public static void clearLocale() {
		clear(LOCALE);
	}

	static {
		noPampasMap = Maps.newHashMap();

		currentEnv = new ThreadLocal<Map<ParanaThreadVars, Object>>() {
			protected Map<ParanaThreadVars, Object> initialValue() {
				return Maps.newHashMap();
			}
		};
	}
}