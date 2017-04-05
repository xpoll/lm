package io.terminus.parana.auth.core;

import com.google.common.collect.Lists;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

public class LoadConfig implements Serializable {
   private String app;
   private String sign;
   private Date loadedAt;
   private String location;
   private List imported = Lists.newArrayList();

   public String getApp() {
      return this.app;
   }

   public void setApp(String app) {
      this.app = app;
   }

   public String getSign() {
      return this.sign;
   }

   public void setSign(String sign) {
      this.sign = sign;
   }

   public Date getLoadedAt() {
      return this.loadedAt;
   }

   public void setLoadedAt(Date loadedAt) {
      this.loadedAt = loadedAt;
   }

   public String getLocation() {
      return this.location;
   }

   public void setLocation(String location) {
      this.location = location;
   }

   public List getImported() {
      return this.imported;
   }

   public void setImported(List imported) {
      this.imported = imported;
   }
}
