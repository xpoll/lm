package cn.blmdz.hunt.engine;

import java.util.Locale;
import java.util.Map;

import com.google.common.base.Objects;

import cn.blmdz.hunt.engine.model.App;

public enum ThreadVars {
	APP,

	DOMAIN,

	PORT,

	LOCALE,

	SEO;

	private static ThreadLocal<Map<ThreadVars, Object>> currentEnv = new ThreadLocal<Map<ThreadVars, Object>>();

	@SuppressWarnings("unchecked")
	private static <T> T get(ThreadVars var) {
		return (T) currentEnv.get().get(var);
	}

	private static void set(ThreadVars var, Object value) {
		currentEnv.get().put(var, value);
	}

	private static void clear(ThreadVars var) {
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
		return (String) Objects.firstNonNull(get(DOMAIN), getApp().getDomain());
	}

	public static void clearDomain() {
		clear(DOMAIN);
	}

	public static void setPort(Integer port) {
		set(PORT, port);
	}

	public static void clearPort() {
		clear(PORT);
	}

	public static String getHost() {
		Integer port = (Integer) Objects.firstNonNull(get(PORT), Integer.valueOf(80));
		if (port.intValue() == 80) {
			return getDomain();
		}
		return getDomain() + ":" + port;
	}

	public static void setLocale(Locale locale) {
		set(LOCALE, locale);
	}

	public static Locale getLocale() {
		return (Locale) Objects.firstNonNull(get(LOCALE), Locale.getDefault());
	}

	public static void clearLocale() {
		clear(LOCALE);
	}
}