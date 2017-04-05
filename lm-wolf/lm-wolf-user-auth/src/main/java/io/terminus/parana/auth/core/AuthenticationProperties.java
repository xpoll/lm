package io.terminus.parana.auth.core;

import io.terminus.parana.auth.model.App;
import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(
   prefix = "parana.auth"
)
public class AuthenticationProperties {
   private boolean devMode = false;
   private String level = "easy";
   private String path;
   private String aclFileName = "acl.yaml";
   private String key;
   private String domain;
   private String host;

   public App getApp() {
      App app = new App();
      app.setAssetsHome(this.path);
      app.setDomain(this.domain);
      app.setKey(this.key);
      return app;
   }

   public boolean isDevMode() {
      return this.devMode;
   }

   public void setDevMode(boolean devMode) {
      this.devMode = devMode;
   }

   public String getLevel() {
      return this.level;
   }

   public void setLevel(String level) {
      this.level = level;
   }

   public String getPath() {
      return this.path;
   }

   public void setPath(String path) {
      this.path = path;
   }

   public String getAclFileName() {
      return this.aclFileName;
   }

   public void setAclFileName(String aclFileName) {
      this.aclFileName = aclFileName;
   }

   public String getKey() {
      return this.key;
   }

   public void setKey(String key) {
      this.key = key;
   }

   public String getDomain() {
      return this.domain;
   }

   public void setDomain(String domain) {
      this.domain = domain;
   }

   public String getHost() {
      return this.host;
   }

   public void setHost(String host) {
      this.host = host;
   }
}
