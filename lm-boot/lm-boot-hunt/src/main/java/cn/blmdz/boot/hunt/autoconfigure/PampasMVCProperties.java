package cn.blmdz.boot.hunt.autoconfigure;

import java.util.List;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(
   prefix = "pampas.mvc"
)
public class PampasMVCProperties {
   private List<Interceptors> ignoreInterceptors;
   private List<String> customInterceptors;
   private String defaultErrorView;
   private Map codeErrorViews;

   public List<Interceptors> getIgnoreInterceptors() {
      return this.ignoreInterceptors;
   }

   public void setIgnoreInterceptors(List ignoreInterceptors) {
      this.ignoreInterceptors = ignoreInterceptors;
   }

   public List<String> getCustomInterceptors() {
      return this.customInterceptors;
   }

   public void setCustomInterceptors(List customInterceptors) {
      this.customInterceptors = customInterceptors;
   }

   public String getDefaultErrorView() {
      return this.defaultErrorView;
   }

   public void setDefaultErrorView(String defaultErrorView) {
      this.defaultErrorView = defaultErrorView;
   }

   public Map getCodeErrorViews() {
      return this.codeErrorViews;
   }

   public void setCodeErrorViews(Map codeErrorViews) {
      this.codeErrorViews = codeErrorViews;
   }

   public static enum Interceptors {
      CSRFCheck,
      App,
      LocaleJudge,
      Cookie,
      Login,
      Auth;
   }
}
