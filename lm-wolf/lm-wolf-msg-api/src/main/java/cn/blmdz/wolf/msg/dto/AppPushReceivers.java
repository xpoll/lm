package cn.blmdz.wolf.msg.dto;

import java.util.ArrayList;
import java.util.List;

public class AppPushReceivers {
   private List android = new ArrayList();
   private List ios = new ArrayList();
   private List wp = new ArrayList();

   public List getAndroid() {
      return this.android;
   }

   public List getIos() {
      return this.ios;
   }

   public List getWp() {
      return this.wp;
   }

   public void setAndroid(List android) {
      this.android = android;
   }

   public void setIos(List ios) {
      this.ios = ios;
   }

   public void setWp(List wp) {
      this.wp = wp;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof AppPushReceivers)) {
         return false;
      } else {
         AppPushReceivers other = (AppPushReceivers)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$android = this.getAndroid();
            Object other$android = other.getAndroid();
            if(this$android == null) {
               if(other$android != null) {
                  return false;
               }
            } else if(!this$android.equals(other$android)) {
               return false;
            }

            Object this$ios = this.getIos();
            Object other$ios = other.getIos();
            if(this$ios == null) {
               if(other$ios != null) {
                  return false;
               }
            } else if(!this$ios.equals(other$ios)) {
               return false;
            }

            Object this$wp = this.getWp();
            Object other$wp = other.getWp();
            if(this$wp == null) {
               if(other$wp != null) {
                  return false;
               }
            } else if(!this$wp.equals(other$wp)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof AppPushReceivers;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $android = this.getAndroid();
      result = result * 31 + ($android == null?0:$android.hashCode());
      Object $ios = this.getIos();
      result = result * 31 + ($ios == null?0:$ios.hashCode());
      Object $wp = this.getWp();
      result = result * 31 + ($wp == null?0:$wp.hashCode());
      return result;
   }

   public String toString() {
      return "AppPushReceivers(android=" + this.getAndroid() + ", ios=" + this.getIos() + ", wp=" + this.getWp() + ")";
   }
}
