package cn.blmdz.wolf.web.pay.event;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.Map;

import cn.blmdz.wolf.pay.model.PayChannel;

public class RefundedEvent implements Serializable {
   private Map params;
   private PayChannel payChannel;

   public Map getParams() {
      return this.params;
   }

   public PayChannel getPayChannel() {
      return this.payChannel;
   }

   public void setParams(Map params) {
      this.params = params;
   }

   public void setPayChannel(PayChannel payChannel) {
      this.payChannel = payChannel;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof RefundedEvent)) {
         return false;
      } else {
         RefundedEvent other = (RefundedEvent)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$params = this.getParams();
            Object other$params = other.getParams();
            if(this$params == null) {
               if(other$params != null) {
                  return false;
               }
            } else if(!this$params.equals(other$params)) {
               return false;
            }

            Object this$payChannel = this.getPayChannel();
            Object other$payChannel = other.getPayChannel();
            if(this$payChannel == null) {
               if(other$payChannel != null) {
                  return false;
               }
            } else if(!this$payChannel.equals(other$payChannel)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof RefundedEvent;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $params = this.getParams();
      result = result * 31 + ($params == null?0:$params.hashCode());
      Object $payChannel = this.getPayChannel();
      result = result * 31 + ($payChannel == null?0:$payChannel.hashCode());
      return result;
   }

   public String toString() {
      return "RefundedEvent(params=" + this.getParams() + ", payChannel=" + this.getPayChannel() + ")";
   }

   @ConstructorProperties({"params", "payChannel"})
   public RefundedEvent(Map params, PayChannel payChannel) {
      this.params = params;
      this.payChannel = payChannel;
   }
}
