package cn.blmdz.boot.hunt.autoconfigure;

import java.util.List;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@ConfigurationProperties(prefix = "pampas.mvc")
public class PampasMVCProperties {
	private List<Interceptors> ignoreInterceptors;
	private List<String> customInterceptors;
	private String defaultErrorView;
	private Map<Integer, String> codeErrorViews;

	public static enum Interceptors {
		CSRFCheck, App, LocaleJudge, Cookie, Login, Auth;
	}
}
