package cn.blmdz.wolf.auth.model;

import java.io.Serializable;

public class App implements Serializable {
   private static final long serialVersionUID = 5958938777546279799L;
   private String key;
   private String domain;
   private String assetsHome;
   private String configPath;

   public boolean equals(Object o) {
      if(this == o) {
         return true;
      } else if(!(o instanceof App)) {
         return false;
      } else {
         App app = (App)o;
         return this.key.equals(app.key);
      }
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

   public String getAssetsHome() {
      return this.assetsHome;
   }

   public void setAssetsHome(String assetsHome) {
      this.assetsHome = assetsHome;
   }

   public String getConfigPath() {
      return this.configPath;
   }

   public void setConfigPath(String configPath) {
      this.configPath = configPath;
   }
}
