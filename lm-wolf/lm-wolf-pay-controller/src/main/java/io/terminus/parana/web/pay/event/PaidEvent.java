package io.terminus.parana.web.pay.event;

import io.terminus.parana.pay.model.PayChannel;
import io.terminus.parana.pay.model.TradePay;
import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

public class PaidEvent implements Serializable {
   private Map params;
   private PayChannel payChannel;
   private List orderIds;
   private TradePay tradePay;

   public Map getParams() {
      return this.params;
   }

   public PayChannel getPayChannel() {
      return this.payChannel;
   }

   public List getOrderIds() {
      return this.orderIds;
   }

   public TradePay getTradePay() {
      return this.tradePay;
   }

   public void setParams(Map params) {
      this.params = params;
   }

   public void setPayChannel(PayChannel payChannel) {
      this.payChannel = payChannel;
   }

   public void setOrderIds(List orderIds) {
      this.orderIds = orderIds;
   }

   public void setTradePay(TradePay tradePay) {
      this.tradePay = tradePay;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof PaidEvent)) {
         return false;
      } else {
         PaidEvent other = (PaidEvent)o;
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

            Object this$orderIds = this.getOrderIds();
            Object other$orderIds = other.getOrderIds();
            if(this$orderIds == null) {
               if(other$orderIds != null) {
                  return false;
               }
            } else if(!this$orderIds.equals(other$orderIds)) {
               return false;
            }

            Object this$tradePay = this.getTradePay();
            Object other$tradePay = other.getTradePay();
            if(this$tradePay == null) {
               if(other$tradePay != null) {
                  return false;
               }
            } else if(!this$tradePay.equals(other$tradePay)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof PaidEvent;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $params = this.getParams();
      result = result * 31 + ($params == null?0:$params.hashCode());
      Object $payChannel = this.getPayChannel();
      result = result * 31 + ($payChannel == null?0:$payChannel.hashCode());
      Object $orderIds = this.getOrderIds();
      result = result * 31 + ($orderIds == null?0:$orderIds.hashCode());
      Object $tradePay = this.getTradePay();
      result = result * 31 + ($tradePay == null?0:$tradePay.hashCode());
      return result;
   }

   public String toString() {
      return "PaidEvent(params=" + this.getParams() + ", payChannel=" + this.getPayChannel() + ", orderIds=" + this.getOrderIds() + ", tradePay=" + this.getTradePay() + ")";
   }

   @ConstructorProperties({"params", "payChannel", "orderIds", "tradePay"})
   public PaidEvent(Map params, PayChannel payChannel, List orderIds, TradePay tradePay) {
      this.params = params;
      this.payChannel = payChannel;
      this.orderIds = orderIds;
      this.tradePay = tradePay;
   }
}
